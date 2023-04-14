module Checker.Infer where

import Checker.Context (Context)
import Checker.Context qualified as C
import Checker.Monad (Checker, errorLabel, extend, fresh, getCtx, modifyCtx, withNewMarker, withSplit, freshName)
import Checker.Subtype (checkWfType, subtype)
import Control.Monad (foldM, forM, forM_)
import Data.Map qualified as M
import Data.Text (Text)
import Debug.Trace
import Error (Pos)
import Kind (Kind)
import Kind qualified
import Module (Module (Module))
import Module qualified as Module
import Syntax.Ast qualified as Ast
import Type (Type, Var)
import Type qualified as T
import qualified Name

typeConv :: Ast.Type -> Checker Type
typeConv t' = do
    ctx <- getCtx
    t <- conv ctx t'
    let pos = Ast.pos t'
    checkWfType pos ctx t
    return t
  where
    conv :: Context -> Ast.Type -> Checker Type
    conv ctx = \case
        Ast.TSymbol p t ->
            checkSymbol ctx p t >>= \case
                (_, Kind.App ks _) ->
                    errorLabel ("type " ++ show t ++ " expects " ++ show (length ks) ++ " arguments") p
                (u, _) -> return u
        Ast.TApp (Ast.TSymbol p t) ts ->
            checkSymbol ctx p t >>= \case
                (u@(T.Base _), Kind.App ks _)
                    | length ks == length ts -> T.App u <$> mapM (conv ctx) ts
                    | otherwise ->
                        errorLabel
                            ( "type constructor expects "
                                ++ show (length ks)
                                ++ " arguments, got "
                                ++ show (length ts)
                            )
                            p
                (u@(T.Var _), _) -> T.App u <$> mapM (conv ctx) ts
                _ -> errorLabel "type constructor expects 0 arguments" p
        Ast.TApp t ts -> T.App <$> conv ctx t <*> mapM (conv ctx) ts
        Ast.TTuple _ [] -> return T.Unit
        Ast.TTuple _ ts -> T.tuple <$> mapM (conv ctx) ts
        Ast.TArrow a b -> T.Arrow <$> conv ctx a <*> conv ctx b

-- qualifySymbol :: Context -> Pos -> Text -> [(Pos, Text)] -> Checker (Type, Kind)
-- qualifySymbol ctx p t [] = checkSymbol (`elem` C.vars ctx) (C.findKind ctx) p t
-- qualifySymbol ctx p t path =
--     let headPath = maybe (Left (p, t)) Right (C.findModule ctx (T.TVar t))
--      in case headPath >>= findPathInModule path of
--             Right (p', t', m) -> checkSymbol (const False) (`M.lookup` Module.module_kinds m) p' t'
--             Left (p', t') -> errorLabel ("undefined module " ++ show t') p'

-- findPathInModule :: [(Pos, Text)] -> Module -> Either (Pos, Text) (Pos, Text, Module)
-- findPathInModule [] _ = undefined
-- findPathInModule [(p, t)] m = return (p, t, m)
-- findPathInModule ((p, t) : ps) m =
--     let m' = maybe (Left (p, t)) Right (M.lookup (T.TVar t) $ Module.module_modules m)
--      in m' >>= findPathInModule ps

checkSymbol :: Context -> Pos -> Text -> Checker (Type, Kind)
checkSymbol ctx p t = do
    let var = T.TVar t
    if var `elem` C.vars ctx
        then return (T.Var var, Kind.Star)
        else case C.findKind ctx t of
            Nothing -> errorLabel ("undefined type " ++ show var) p
            Just (n, k) -> return (T.Base n, k)

instantiateType :: Type -> Checker Type
instantiateType = \case
    T.Forall alpha a -> do
        alphaE <- fresh
        extend [C.Exist alphaE]
        let a' = T.substitute (T.Exist alphaE) alpha a
        instantiateType a'
    t -> return t

instantiatePattern :: Type -> Ast.Pattern -> Checker ()
instantiatePattern scrutinee = \case
    Ast.PVar _ x -> do
        name <- freshName x 
        extend [C.Term C.Private $ C.Type name scrutinee]
    Ast.PApp p x patterns -> do
        ctx <- getCtx
        conTy <- case C.findType ctx x of
            Just t -> return t
            Nothing -> errorLabel ("undefined constructor " ++ show x) p
        instantiatePatternApp p scrutinee conTy patterns
    t@(Ast.PTup _ patterns) -> do
        let p = Ast.pos t
        ctx <- getCtx
        instantiatePatternTuple p (C.apply ctx scrutinee) patterns
    Ast.PAnn pat anno' -> do
        let p = Ast.pos anno'
        anno <- typeConv anno'
        subtype p anno scrutinee
        ctx <- getCtx
        instantiatePattern (C.apply ctx anno) pat

instantiatePatternTuple :: Pos -> Type -> [Ast.Pattern] -> Checker ()
instantiatePatternTuple p a@T.Forall{} patterns = do
    a' <- instantiateType a
    instantiatePatternTuple p a' patterns
instantiatePatternTuple _ T.Unit [] = return ()
instantiatePatternTuple p (T.Tuple scrutinees) patterns
    | length scrutinees == length patterns =
        forM_ (zip scrutinees patterns) (uncurry instantiatePattern)
    | otherwise = errorLabel ("expecting a tuple pattern with " ++ show (length scrutinees) ++ " items") p
instantiatePatternTuple p scrutinee _ =
    errorLabel ("expected " ++ T.pretty scrutinee) p

instantiatePatternApp :: Pos -> Type -> Type -> [Ast.Pattern] -> Checker ()
instantiatePatternApp p scrutinee a@T.Forall{} args = do
    a' <- instantiateType a
    instantiatePatternApp p scrutinee a' args
instantiatePatternApp p scrutinee c [] = do
    subtype p c scrutinee
instantiatePatternApp p scrutinee (T.Arrow (T.Tuple params) c) args
    | length params == length args = do
        subtype p c scrutinee
        forM_ (zip params args) (uncurry instantiatePattern)
    | otherwise = errorLabel ("constructor expects " ++ show (length params) ++ " arguments, got " ++ show (length args)) p
instantiatePatternApp p scrutinee (T.Arrow a c) [arg] = do
    subtype p c scrutinee
    instantiatePattern a arg
instantiatePatternApp p _ (T.Arrow _ _) args = errorLabel ("constructor expects 1 argument, got " ++ show (length args)) p
instantiatePatternApp p _ _ args =
    errorLabel ("constructor expects 0 arguments, got " ++ show (length args)) p

instantiateConstraint :: Ast.Constraint -> Checker ()
instantiateConstraint = \case
    Ast.CVar _ t -> extend [C.Var (T.TVar t)]

applyConstraint :: Ast.Constraint -> Type -> Type
applyConstraint c t = case c of
    Ast.CVar _ x -> T.Forall (T.TVar x) t

constraintType :: Ast.Constraint -> Type
constraintType = \case
    Ast.CVar _ x -> T.Var (T.TVar x)

check :: Ast.Expr -> Type -> Checker ()
check e' t' = case (e', t') of
    (e, b) -> do
        a <- synth e
        let eP = Ast.pos e
        ctx <- getCtx
        subtype eP (C.apply ctx a) (C.apply ctx b)

synth :: Ast.Expr -> Checker Type
synth = \case
    Ast.Def _ name' constraints params' retTy' e e' -> do
        name <- freshName name'
        funcTy <- withNewMarker $ do
            -- first, instantiate constraints into scope
            forM_ constraints instantiateConstraint
            params <- forM params' (traverse typeConv)
            let paramTys = map snd params
            retTy <- typeConv retTy'
            let funcTy' = T.Arrow (T.tuple paramTys) retTy
            let funcTy = foldr applyConstraint funcTy' constraints
            -- bring function into scope now to allow for recursion
            extend [C.Term C.Public $ C.Type name funcTy]
            -- bring parameters into scope
            forM_ params $ uncurry (flip instantiatePattern)
            check e retTy
            return funcTy
        extend [C.Term C.Public $ C.Type name funcTy]
        synth e'
    Ast.Type _ name' constraints constructors e -> do
        name <- freshName name'
        let thisTy = T.app (T.Base name) (map constraintType constraints)
        let thisKind = Kind.app (map (const Kind.Star) constraints) Kind.Star
        extend [C.Term C.Public $ C.Kind name thisKind]
        elems <- withNewMarker $ do
            forM_ constraints instantiateConstraint
            forM constructors (synthConstructor thisTy constraints)
        extend elems
        synth e
    Ast.Call e args -> do
        funcTy <- synth e
        synthApp (Ast.pos e) funcTy args
    Ast.Symbol p t -> do
        ctx <- getCtx
        case C.findType ctx t of
            Just t' -> return t'
            Nothing -> errorLabel ("undefined symbol " ++ show t) p
    Ast.Module _ t e e' -> do
        (_, ctx) <- withSplit $ do
            synth e
        let m = makeModule ctx
        name <- freshName t
        extend [C.Term C.Public $ C.Module name m]
        let qualifiedM = Module.qualifyModule t m
        modifyCtx (openModule qualifiedM <>)
        synth e'
    Ast.Open p t e -> do
        ctx <- getCtx
        case C.findModule ctx t of
            Just m -> do
                modifyCtx (openModule m <>)
                synth e
            Nothing -> errorLabel ("undefined module " ++ show t) p
    Ast.Match _ scrutinee branches -> do
        alpha <- fresh
        extend [C.Exist alpha]

        let matchTy = T.Exist alpha
        scrutineeTy <- synth scrutinee

        forM_ branches $ \(Ast.Branch pat e) -> withNewMarker $ do
            instantiatePattern scrutineeTy pat
            ctx <- getCtx
            check e (C.apply ctx matchTy)
        
        ctx <- getCtx
        return $ C.apply ctx matchTy
    Ast.Numeric{} -> return (T.Base $ Name.Builtin "Int")
    Ast.Tuple _ [] -> return T.Unit
    Ast.Unit -> return T.Unit
    e -> error $ "synth unimplemented for " ++ show e

makeModule :: Context -> Module
makeModule ctx =
    Module
        { Module.module_kinds = M.fromList (C.explicitKinds ctx)
        , Module.module_types = M.fromList (C.explicitTypes ctx)
        , Module.module_modules = M.fromList (C.explicitModules ctx)
        }

openModule :: Module -> Context
openModule m =
    let kinds = mapToList C.Kind (Module.module_kinds m)
        terms = mapToList C.Type (Module.module_types m)
        modules = mapToList C.Module (Module.module_modules m)
     in C.Context $ map (C.Term C.Private) $ kinds <> terms <> modules
  where
    mapToList con = M.foldrWithKey (\k v -> (con k v :)) []

synthConstructor :: Type -> [Ast.Constraint] -> Ast.Constructor -> Checker C.Elem
synthConstructor thisTy constraints = \case
    Ast.Constructor _ name' params' -> do
        name <- freshName name'
        params <- mapM typeConv params'
        let arrow = case params of
                [] -> thisTy
                ts -> T.Arrow (T.tuple ts) thisTy
        let constructorTy = foldr applyConstraint arrow constraints
        return $ C.Term C.Public $ C.Type name constructorTy

synthApp :: Pos -> Type -> [Ast.Expr] -> Checker Type
synthApp p (T.Forall alpha a) args = do
    alphaE <- fresh
    extend [C.Exist alphaE]
    synthApp p (T.substitute (T.Exist alphaE) alpha a) args
synthApp _ (T.Arrow T.Unit c) [] = return c
synthApp p (T.Arrow (T.Tuple params) c) args
    | length params == length args = do
        forM_ (zip args params) $ uncurry check
        return c
    | otherwise = errorLabel ("function expects " ++ show (length params) ++ " arguments, got " ++ show (length args)) p
synthApp _ (T.Arrow a c) [arg] = do
    check arg a
    return c
synthApp p (T.Arrow _ _) args = errorLabel ("function expects 1 argument, got " ++ show (length args)) p
synthApp p funcTy _ =
    errorLabel ("cannot call a non-function, specifically a " ++ T.pretty funcTy) p