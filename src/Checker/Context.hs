module Checker.Context where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transform)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Kind (Kind (Star), app)
import Module (Module)
import Unique (Name (Builtin), isName)
import Type (Existential, Type, Var)
import Type qualified as T

data Term 
    = Type Name Type
    | Kind Name Kind
    | Module Name Module
    deriving (Show, Data, Typeable)

-- whether a term gets moved into a new module
-- e.g `open T` imports *private* terms into the module so to prevent shadowing
data AccessModifier = Public | Private
    deriving (Show, Data, Typeable)

data Elem
    = Var Var
    | Term AccessModifier Term
    | Exist Existential
    | Solved Existential Type
    | Marker Existential
    deriving (Show, Data, Typeable)

newtype Context = Context [Elem]
    deriving (Show)

instance Semigroup Context where
    (Context a) <> (Context b) = Context $ a <> b

elemPretty :: Elem -> String
elemPretty = \case
    Var alpha -> show alpha
    Exist alpha -> show alpha
    Marker alpha -> "@" ++ show alpha
    Solved alpha t -> show alpha ++ " = " ++ T.pretty t
    Term _ (Type x t) -> show x ++ " : " ++ T.pretty t
    Term _ (Kind x k) -> show x ++ " : " ++ show k
    Term _ (Module x _) -> "module " ++ show x

exists :: Context -> [Existential]
exists (Context ctx) =
    ctx >>= \case
        Exist alpha -> [alpha]
        Solved alpha _ -> [alpha]
        _ -> []

builtins :: Context
builtins =
    Context
        [ builtin "->" (Kind.app [Star, Star] Star)
        , builtin "Int" Star
        ]
  where
    builtin name = Term Private . Kind (Unique.Builtin name)

vars :: Context -> [Var]
vars (Context ctx) = [alpha | Var alpha <- ctx]

explicitKinds :: Context -> [(Name, Kind)]
explicitKinds (Context ctx) = [(v, k) | Term Public (Kind v k) <- ctx]

explicitTypes :: Context -> [(Name, Type)]
explicitTypes (Context ctx) = [(v, t) | Term Public (Type v t) <- ctx]

explicitModules :: Context -> [(Name, Module)]
explicitModules (Context ctx) = [(v, m) | Term Public (Module v m) <- ctx]

wf :: Context -> Type -> Bool
wf ctx = \case
    T.Base{} -> True
    T.Var alpha -> alpha `elem` vars ctx
    T.Exist alpha -> alpha `elem` exists ctx
    T.Forall alpha a -> wf (ctx <+ Var alpha) a
    T.App a b -> wf ctx a && all (wf ctx) b
    T.Tuple ts -> all (wf ctx) ts

pretty :: Context -> String
pretty (Context ctx) = "Γ[" ++ intercalate ", " (map elemPretty $ reverse ctx) ++ "]"

(<+) :: Context -> Elem -> Context
(<+) (Context ctx) e = Context $ e : ctx

(<++) :: Context -> [Elem] -> Context
(<++) (Context ctx) e = Context $ reverse e <> ctx

onExistential :: Existential -> Elem -> Bool
onExistential alpha (Exist beta) | alpha == beta = True
onExistential _ _ = False

onMarker :: Existential -> Elem -> Bool
onMarker alpha (Marker beta) | alpha == beta = True
onMarker _ _ = False

onVar :: Var -> Elem -> Bool
onVar alpha (Var beta) | alpha == beta = True
onVar _ _ = False

splitOn :: (Elem -> Bool) -> Context -> (Context, Context)
splitOn on (Context ctx) = let (l, _ : r) = break on ctx in (Context l, Context r)

dropOn :: (Elem -> Bool) -> Context -> Context
dropOn on (Context ctx) = Context $ tail $ dropWhile (not . on) ctx

insertOn :: (Elem -> Bool) -> [Elem] -> Context -> Context
insertOn on e ctx = let (l, r) = splitOn on ctx in l <> (r <++ e)

unsolveds :: Context -> [Existential]
unsolveds (Context ctx) = [alpha | Exist alpha <- ctx]

orderedSolve :: Existential -> Existential -> Context -> Context
orderedSolve alpha beta ctx = case splitOn (onExistential alpha) ctx of
    (l, r) | beta `elem` unsolveds r -> l <> (r <+ Solved alpha (T.Exist beta))
    (l, r) -> insertOn (onExistential beta) [Solved beta (T.Exist alpha)] l <> (r <+ Exist alpha)

instSolve :: Existential -> Type -> Context -> Maybe Context
instSolve alpha t ctx = case splitOn (onExistential alpha) ctx of
    (g', g) | wf g t -> Just $ g' <> (g <+ Solved alpha t)
    _ -> Nothing

findType :: Context -> Text -> Maybe (Name, Type)
findType (Context ctx) alpha =
    listToMaybe [(name, t) | Term _ (Type name t) <- ctx, Unique.isName alpha name]

findModule :: Context -> Text -> Maybe Module
findModule (Context ctx) alpha =
    listToMaybe [t | Term _ (Module name t) <- ctx, Unique.isName alpha name]

findKind :: Context -> Text -> Maybe (Name, Kind)
findKind (Context ctx) alpha =
    listToMaybe [(name, t) | Term _ (Kind name t) <- ctx, Unique.isName alpha name]

findSolved :: Context -> Existential -> Maybe Type
findSolved (Context ctx) alpha = listToMaybe [t | Solved beta t <- ctx, alpha == beta]

apply :: Context -> Type -> Type
apply ctx = transform aux
  where
    aux (T.Exist alpha) = case findSolved ctx alpha of
        Just t -> apply ctx t
        Nothing -> T.Exist alpha
    aux a = a