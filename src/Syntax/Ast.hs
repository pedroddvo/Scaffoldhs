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

data Constructor
    = Constructor Pos Text [Type]
    deriving (Data, Typeable, Show)

data Expr
    = Symbol Pos Text
    | Numeric Pos Text
    | Def Pos Text [Constraint] [(Pattern, Type)] Type Expr Expr
    | Tuple Pos [Expr]
    | Call Expr [Expr]
    | Type Pos Text [Constraint] [Constructor] Expr
    | Module Pos Text Expr Expr
    | Open Pos Text Expr
    | -- | Only appears as a terminator for Expr
      Unit
    deriving (Data, Typeable, Show)

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

instance Spanned Expr where
    pos = \case
        Symbol p _ -> p
        Numeric p _ -> p
        Def p _ _ _ _ _ _ -> p
        Type p _ _ _ _ -> p
        Tuple p es -> foldl' (<>) p (map pos es)
        Call e es -> foldl' (<>) (pos e) (map pos es)
        Unit -> undefined