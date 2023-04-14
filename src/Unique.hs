module Unique where

import Data.Data (Data, Typeable)
import Data.Text (Text)
import Data.Text qualified as T

type Id = Int

data Unique = Unique
    { unique_id :: !Id
    , unique_name :: !Text
    }
    deriving (Ord, Data, Typeable)

data Name
    = Builtin Text
    | Name Unique
    deriving (Ord, Data, Typeable)

instance Show Unique where
    show uniq = T.unpack $ unique_name uniq

instance Eq Unique where
    a == b = unique_id a == unique_id b

instance Eq Name where
    (Name id1) == (Name id2) = id1 == id2
    (Builtin t) == (Builtin t') = t == t'
    _ == _ = False

instance Show Name where
    show (Name t) = show t
    show (Builtin t) = T.unpack t

isName :: Text -> Name -> Bool
isName t (Builtin t') = t == t'
isName t (Name t') = t == unique_name t'

qualify :: Text -> Name -> Name
qualify path (Name uniq) = Name $ Unique (unique_id uniq) (path <> "." <> unique_name uniq)
qualify _ n = n