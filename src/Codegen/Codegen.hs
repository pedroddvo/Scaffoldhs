{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Codegen.Codegen where

import Codegen.Types (typeToLLVM)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.State (MonadState, State, execState, modify)
import Data.Text (Text)
import Data.Text.Short qualified as TS
import LLVM.AST (BasicBlock, Definition (GlobalDefinition))
import LLVM.AST qualified as LLVM
import LLVM.AST.Global qualified as Def
import LLVM.AST.Type (i32)
import LLVM.IRBuilder (IRBuilder, IRBuilderT, ModuleBuilder, MonadIRBuilder, function, int32, ret)
import LLVM.IRBuilder qualified as IR
import LLVM.Prelude (ShortByteString)
import Module (Module (module_name))
import Syntax.Ast qualified as Ast
import Type (Type)
import Unique (Name)
import Unique qualified

newtype LLVM a = LLVM (State LLVM.Module a)
    deriving (Functor, Applicative, Monad, MonadState LLVM.Module)

data CodegenState = CodegenState
    { codegen_module :: Module
    }

newtype CodegenM a = CodegenM {unCodegenM :: ReaderT CodegenState (IRBuilderT ModuleBuilder) a}
    deriving (Functor, Applicative, Monad, MonadReader CodegenState, MonadIRBuilder)

runLLVM :: LLVM.Module -> LLVM a -> LLVM.Module
runLLVM m (LLVM a) = execState a m

makeShortBytestring :: Text -> ShortByteString
makeShortBytestring = TS.toShortByteString . TS.fromText

makeName :: Name -> LLVM.Name
makeName = LLVM.Name . makeShortBytestring . Unique.nameText

addDefn :: Definition -> LLVM ()
addDefn d = modify $ \s ->
    s{LLVM.moduleDefinitions = LLVM.moduleDefinitions s ++ [d]}

define :: Name -> Type -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define funcName retTy params body =
    addDefn $
        GlobalDefinition $
            LLVM.functionDefaults
                { Def.name = makeName funcName
                , Def.parameters = (map makeParam params, False)
                , Def.returnType = typeToLLVM retTy
                , Def.basicBlocks = body
                }
  where
    makeParam (t, name) =
        Def.Parameter (typeToLLVM t) (makeName name) []

genExpr :: Ast.Expr Name -> CodegenM ()
genExpr = \case
    Ast.Def _ name _ _ _ e e' -> undefined

compileModule :: Module -> ModuleBuilder ()
compileModule m = do
    function "main" [] i32 $ \_ -> do
        ret (int32 1337)
    return ()

buildModule :: Module -> LLVM.Module
buildModule m =
    let name = makeShortBytestring (Unique.nameText $ module_name m)
    in IR.buildModule name $ compileModule m

runCodegen :: Module -> CodegenM a -> IRBuilderT ModuleBuilder a
runCodegen m =
    flip runReaderT (CodegenState m)
        . unCodegenM