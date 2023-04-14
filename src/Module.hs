module Module where

import Data.Data (Data, Typeable)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Kind (Kind)
import Name (Name, qualify)
import Type (Type, Var)
import Type qualified as T

data Module = Module
    { module_types :: Map Name Type
    , module_kinds :: Map Name Kind
    , module_modules :: Map Name Module
    }
    deriving (Show, Data, Typeable)

moduleNames :: Module -> Set Name
moduleNames m =
    M.keysSet (module_types m)
        <> M.keysSet (module_kinds m)
        <> S.unions (map moduleNames $ M.elems (module_modules m))

qualifyModule :: Text -> Module -> Module
qualifyModule path m =
    let names = moduleNames m
     in Module
            { module_types = M.mapKeys (Name.qualify path) (M.map (T.qualifyType names path) $ module_types m)
            , module_kinds = M.mapKeys (Name.qualify path) (module_kinds m)
            , module_modules = M.mapKeys (Name.qualify path) (M.map (qualifyModule path) $ module_modules m)
            }