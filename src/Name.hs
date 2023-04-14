module Name where

import Data.Data (Data, Typeable)
import Data.Text (Text)
import Data.Text qualified as T

type Id = Int

data Name = Builtin Text
          | Name Text Id
    deriving (Ord, Data, Typeable)

instance Eq Name where
    (Name _ id1) == (Name _ id2) = id1 == id2
    (Builtin t) == (Builtin t') = t == t'
    _ == _ = False

instance Show Name where
    show (Name t _) = T.unpack t
    show (Builtin t) = T.unpack t

isName :: Text -> Name -> Bool
isName t (Builtin t') = t == t'
isName t (Name t' _) = t == t'

qualify :: Text -> Name -> Name
qualify path (Name t i) = Name (path <> "." <> t) i
qualify _ n = n