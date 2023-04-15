{-# LANGUAGE DeriveTraversable #-}

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
    = Constructor Pos a [Type]
    deriving (Functor, Foldable, Traversable, Data, Typeable, Show)

data Expr a
    = Symbol Pos a
    | Numeric Pos Text
    | Def Pos a [Constraint] [(Pattern, Type)] Type (Expr a) (Expr a)
    | Tuple Pos [Expr a]
    | Call (Expr a) [Expr a]
    | Type Pos a [Constraint] [Constructor a] (Expr a)
    | Module Pos a (Expr a) (Expr a)
    | Open Pos a (Expr a)
    | Match Pos (Expr a) [Branch a]
    | -- | Only appears as a terminator for (Expr a)
      Unit
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
        Symbol p _ -> p
        Numeric p _ -> p
        Def p _ _ _ _ _ _ -> p
        Module p _ _ _ -> p
        Open p _ _ -> p
        Type p _ _ _ _ -> p
        Tuple p es -> foldl' (<>) p (map pos es)
        Call e es -> foldl' (<>) (pos e) (map pos es)
        Match p _ _ -> p
        Unit -> undefined