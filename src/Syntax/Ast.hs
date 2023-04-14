module Syntax.Ast where

import Data.Data (Data, Typeable)
import Data.Foldable (foldl')
import Data.Text (Text)
import Error (Pos)

data Type
    = TSymbol Pos Text
    | TArrow Type Type
    | TApp Type [Type]
    | TTuple Pos [Type]
    deriving (Data, Typeable, Show)

data Pattern
    = PVar Pos Text
    | PApp Pos Text [Pattern]
    | PTup Pos [Pattern]
    | PAnn Pattern Type
    deriving (Data, Typeable, Show)

data Constraint
    = CVar Pos Text
    deriving (Data, Typeable, Show)

data Constructor a
    = Constructor a Pos Text [Type]
    deriving (Functor, Foldable, Traversable, Data, Typeable, Show)

data Expr a
    = Symbol a Pos Text
    | Numeric a Pos Text
    | Def a Pos Text [Constraint] [(Pattern, Type)] Type (Expr a) (Expr a)
    | Tuple a Pos [Expr a]
    | Call a (Expr a) [Expr a]
    | Type a Pos Text [Constraint] [Constructor a] (Expr a)
    | Module a Pos Text (Expr a) (Expr a)
    | Open a Pos Text (Expr a)
    | Match a Pos (Expr a) [Branch a]
    | -- | Only appears as a terminator for (Expr a)
      Unit a
    deriving (Functor, Foldable, Traversable, Data, Typeable, Show)

data Branch a
    = Branch Pattern (Expr a)
    deriving (Functor, Foldable, Traversable, Data, Typeable, Show)

class Spanned a where
    pos :: a -> Pos

instance Spanned Type where
    pos = \case
        TSymbol p _ -> p
        TArrow a b -> pos a <> pos b
        TApp a ts -> foldl' (<>) (pos a) (map pos ts)
        TTuple p ts -> foldl' (<>) p (map pos ts)

instance Spanned Pattern where
    pos = \case
        PVar p _ -> p
        PApp p _ ps -> foldl' (<>) p (map pos ps)
        PTup p ps -> foldl' (<>) p (map pos ps)
        PAnn p t -> pos p <> pos t

instance Spanned (Expr a) where
    pos = \case
        Symbol _ p _ -> p
        Numeric _ p _ -> p
        Def _ p _ _ _ _ _ _ -> p
        Module _ p _ _ _ -> p
        Open _ p _ _ -> p
        Type _ p _ _ _ _ -> p
        Tuple _ p es -> foldl' (<>) p (map pos es)
        Call _ e es -> foldl' (<>) (pos e) (map pos es)
        Match _ p _ _ -> p
        Unit _ -> undefined