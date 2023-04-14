module Kind where

import Data.Data (Data)
import Data.List (intercalate)

data Kind = Star | App [Kind] Kind
    deriving (Eq, Data)

instance Show Kind where
    show = \case
        Star -> "Type"
        App ks k -> "(" ++ intercalate ", " (map show ks) ++ ") -> " ++ show k

app :: [Kind] -> Kind -> Kind
app [] k = k
app ks k = App ks k