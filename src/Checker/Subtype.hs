module Checker.Subtype (checkWfType, subtype) where

import Checker.Context (Context)
import Checker.Context qualified as C
import Checker.Monad (Checker, CheckerError (MismatchError), errorLabel, extend, fresh, getCtx, modifyCtx, setCtx, withMarker, withNewMarker)
import Control.Monad (forM, forM_)
import Control.Monad.Except (MonadError (catchError), throwError, unless)
import Error (Pos)
import Type (Existential, Type)
import Type qualified as T

checkWfType :: Pos -> Context -> Type -> Checker ()
checkWfType p ctx t =
    unless (C.wf ctx t) (errorLabel ("invalid type " ++ T.pretty t) p)

subtype :: Pos -> Type -> Type -> Checker ()
subtype p a b = catchError (subtype' p a b) $ \err -> case err of
    MismatchError -> do
        ctx <- getCtx
        errorLabel
            ( "expected "
                ++ T.pretty (C.apply ctx b)
                ++ ", got "
                ++ T.pretty (C.apply ctx a)
            )
            p
    _ -> throwError err

subtype' :: Pos -> Type -> Type -> Checker ()
subtype' p a' b' = do
    ctx <- getCtx
    checkWfType p ctx a'
    checkWfType p ctx b'
    case (a', b') of
        (T.Var a, T.Var b) | a == b -> return ()
        (T.Exist a, T.Exist b) | a == b -> return ()
        (T.Base a, T.Base b) | a == b -> return ()
        (T.Arrow a1 a2, T.Arrow b1 b2) -> do
            subtype' p b1 a1
            ctx' <- getCtx
            subtype' p (C.apply ctx' a2) (C.apply ctx' b2)
        (T.App a1 a2, T.App b1 b2) | length a2 == length b2 -> do
            subtype' p a1 b1
            forM_ (zip a2 b2) $ \(a2', b2') -> do
                ctx' <- getCtx
                subtype' p (C.apply ctx' a2') (C.apply ctx' b2')
        (T.Tuple ts1, T.Tuple ts2) | length ts1 == length ts2 -> do
            forM_ (zip ts1 ts2) $ \(t1, t2) -> do
                ctx' <- getCtx
                subtype' p (C.apply ctx' t1) (C.apply ctx' t2)
        (T.Forall alpha a, b) -> do
            alphaE <- fresh
            withMarker alphaE $ do
                extend [C.Exist alphaE]
                subtype' p (T.substitute (T.Exist alphaE) alpha a) b
        (a, T.Forall alpha b) -> do
            withNewMarker $ do
                extend [C.Var alpha]
                subtype' p a b
        (T.Exist alpha, a)
            | alpha `notElem` T.existentials a ->
                instantiateL alpha a
        (a, T.Exist alpha)
            | alpha `notElem` T.existentials a ->
                instantiateR a alpha
        _ -> throwError MismatchError

-- | Γ[α^₂, α^₁, α^ = α^₁ -> α^₂]
solveAsApp :: (Type -> Type -> Type) -> Existential -> Checker (Existential, Existential)
solveAsApp con alpha = do
    alpha1 <- fresh
    alpha2 <- fresh
    let solved =
            [ C.Exist alpha2
            , C.Exist alpha1
            , C.Solved alpha (con (T.Exist alpha1) (T.Exist alpha2))
            ]
    modifyCtx (C.insertOn (C.onExistential alpha) solved)
    return (alpha1, alpha2)

solveMany :: ([Type] -> Type) -> [Type] -> Existential -> Checker [Existential]
solveMany con ts alpha = do
    alphas <- forM ts (const fresh)
    let solved = map C.Exist alphas <> [C.Solved alpha (con $ map T.Exist alphas)]
    modifyCtx (C.insertOn (C.onExistential alpha) solved)
    return alphas


instantiateR :: Type -> T.Existential -> Checker ()
instantiateR t alpha = do
    ctx <- getCtx
    case C.instSolve alpha t ctx of
        Just ctx' -> setCtx ctx'
        Nothing -> case t of
            T.Exist beta -> setCtx $ C.orderedSolve alpha beta ctx
            T.Tuple ts -> do
                alphas <- solveMany T.Tuple ts alpha
                forM_ (zip ts alphas) (uncurry instantiateR)
            T.Arrow a1 a2 -> do
                (alpha1, alpha2) <- solveAsApp T.Arrow alpha
                instantiateL alpha1 a1
                ctx' <- getCtx
                instantiateR (C.apply ctx' a2) alpha2
            T.App a ts -> do
                alphaT <- fresh
                modifyCtx (C.insertOn (C.onExistential alpha) [C.Exist alphaT, C.Exist alpha])
                instantiateR a alphaT
                alphas <- solveMany (T.App $ T.Exist alphaT) ts alpha
                forM_ (zip ts alphas) (uncurry instantiateR)
            T.Forall beta b -> do
                betaE <- fresh
                withMarker betaE $ do
                    extend [C.Exist betaE]
                    instantiateR (T.substitute (T.Exist betaE) beta b) alpha
            _ -> error $ "instantiateL " ++ show alpha ++ ": " ++ show t

instantiateL :: T.Existential -> Type -> Checker ()
instantiateL alpha t = do
    ctx <- getCtx
    case C.instSolve alpha t ctx of
        Just ctx' -> setCtx ctx'
        Nothing -> case t of
            T.Exist beta -> setCtx $ C.orderedSolve alpha beta ctx
            T.Tuple ts -> do
                alphas <- solveMany T.Tuple ts alpha
                forM_ (zip alphas ts) (uncurry instantiateL)
            T.Arrow a1 a2 -> do
                (alpha1, alpha2) <- solveAsApp T.Arrow alpha
                instantiateR a1 alpha1
                ctx' <- getCtx
                instantiateL alpha2 (C.apply ctx' a2)
            T.App a ts -> do
                alphaT <- fresh
                modifyCtx (C.insertOn (C.onExistential alpha) [C.Exist alphaT, C.Exist alpha])
                instantiateL alphaT a
                alphas <- solveMany (T.App $ T.Exist alphaT) ts alpha
                forM_ (zip alphas ts) (uncurry instantiateL)
            T.Forall beta b -> do
                withNewMarker $ do
                    extend [C.Var beta]
                    instantiateL alpha b
            _ -> error $ "instantiateL " ++ show alpha ++ ": " ++ show t