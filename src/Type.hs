module Type where

import Data.Data (Data, Typeable)
import Data.Foldable (foldl')
import Data.Generics.Uniplate.Data (Uniplate, children, para, rewrite, transform, universe)
import Data.List (intercalate)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Debug.Trace (traceShow)
import Name (Name (Builtin, Name), qualify)

newtype Var = TVar Text
    deriving (Eq, Data, Typeable, Ord)

instance Show Var where
    show (TVar t) = T.unpack t

newtype Existential = Existential Int
    deriving (Eq, Data, Typeable, Ord, Num)

unExistential :: Existential -> Int
unExistential (Existential a) = a

instance Show Existential where
    show (Existential e) = show e ++ "^"

data Type
    = Var Var
    | Base Name
    | Forall Var Type
    | App Type [Type]
    | Tuple [Type]
    | Exist Existential
    deriving (Data, Typeable, Show)

pattern Arrow :: Type -> Type -> Type
pattern Arrow a b = App (Base (Name.Builtin "->")) [a, b]

pattern Unit :: Type
pattern Unit = Base (Name.Builtin "Unit")

instance Semigroup Var where
    (TVar a) <> (TVar b) = TVar $ a <> b

pretty :: Type -> String
pretty = \case
    Var alpha -> show alpha
    Exist alpha -> show alpha
    Base a -> show a
    Arrow a@Arrow{} b -> "(" ++ pretty a ++ ") -> " ++ pretty b
    Arrow a b -> pretty a ++ " -> " ++ pretty b
    a@Forall{} -> "∀ " ++ prettyForall a
    App f args -> pretty f ++ "[" ++ intercalate ", " (map pretty args) ++ "]"
    Tuple ts -> "(" ++ intercalate ", " (map pretty ts) ++ ")"
  where
    prettyForall (Forall alpha a) = show alpha ++ " " ++ prettyForall a
    prettyForall a = ". " ++ pretty a

app :: Type -> [Type] -> Type
app t [] = t
app t ts = App t ts

{- | Tuple arity of functions
| e.g arity (a, b, c) -> d == 3
-}
arity :: Type -> Int
arity (Forall _ a) = arity a
arity (Arrow (Tuple n) _) = length n
arity (Arrow _ _) = 1
arity _ = 0

tuple :: [Type] -> Type
tuple [] = Unit
tuple [x] = x
tuple xs = Tuple xs

foralls :: Type -> [(Var, Type)]
foralls u = [(v, t) | Forall v t <- universe u]

isMonotype :: Type -> Bool
isMonotype = null . foralls

freeVars :: Type -> Set Var
freeVars = \case
    Var alpha -> Set.singleton alpha
    Forall alpha a -> Set.delete alpha $ freeVars a
    App a b -> Set.union (freeVars a) (Set.unions $ map freeVars b)
    Tuple as -> Set.unions (map freeVars as)
    _ -> Set.empty

substitute :: Type -> Var -> Type -> Type
substitute b alpha = \case
    Var beta | alpha == beta -> b
    Forall beta e -> Forall beta $ if alpha == beta then e else substitute b alpha e
    App x y -> App (substitute b alpha x) (map (substitute b alpha) y)
    Tuple xs -> Tuple (map (substitute b alpha) xs)
    e -> e

existentials :: Type -> [Existential]
existentials t = [alpha | Exist alpha <- universe t]

qualifyType :: Set Name -> Text -> Type -> Type
qualifyType moduleNames path = transform aux
  where
    aux (Base name)
        | S.member name moduleNames =
            Base $ Name.qualify path name
    aux t = t