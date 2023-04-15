module Pipeline where

import Checker.Context qualified as Context
import Checker.Infer qualified as Infer
import Checker.Monad (CheckerState, runChecker)
import Checker.Monad qualified as State
import Codegen.Codegen qualified as Codegen
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace (traceM, traceShowM)
import Error (Error, errorFinish)
import LLVM (File (File), withModuleFromAST, writeLLVMAssemblyToFile)
import LLVM.CodeGenOpt qualified as Opt
import LLVM.CodeModel qualified as C
import LLVM.Context (withContext)
import LLVM.Internal.Target (withHostTargetMachine)
import LLVM.Relocation qualified as R
import Module (Module)
import Syntax.Ast qualified as Ast
import Syntax.Parser qualified as Parser
import Text.Megaparsec qualified as MP
import Type (Type)
import Unique (Name)
import Checker.Infer (synthToModule)

parseProgram :: Text -> Either (MP.ParseErrorBundle Text Void) (Ast.Expr Text)
parseProgram = MP.parse Parser.program ""

testParseProgram :: Text -> IO ()
testParseProgram src = case parseProgram src of
  Left err -> putStrLn $ MP.errorBundlePretty err
  Right e -> print e

synthExpr :: Ast.Expr Text -> (Either Error (Type, Ast.Expr Name), CheckerState)
synthExpr = runChecker . Infer.synth

testSynthProgram :: Text -> IO ()
testSynthProgram src = case attempt of
  Left err -> putStrLn err
  Right e -> print e
 where
  attempt = do
    e <- first MP.errorBundlePretty $ parseProgram src
    traceShowM e
    let (t, state) = synthExpr e
    traceM (Context.pretty $ State.cs_ctx state)
    first (errorFinish src) t

-- testCodegenProgram :: Text -> IO ()
-- testCodegenProgram src = case attempt of
--     Left err -> putStrLn err
--     Right e -> print e
--   where
--     attempt = do
--         e <- first MP.errorBundlePretty $ parseProgram src
--         traceShowM e
--         let (t, state) = synthExpr e
--         traceM (Context.pretty $ State.cs_ctx state)
--         first (errorFinish src) t

-- codegenExpr :: Ast.Expr Name -> Module -> IO ()
-- codegenExpr = Codegen.genExpr

compile :: Ast.Expr Name -> Module -> IO ()
compile e m' = do
  let m = Codegen.buildModule m'

  withContext $ \ctx -> do
    withModuleFromAST ctx m $ \mod' -> do
      let opt = Opt.None
      withHostTargetMachine R.PIC C.Default opt $ \tm -> do
        writeLLVMAssemblyToFile (File "example.ll") mod'

testCompile :: Text -> IO ()
testCompile src = case attempt of
  Left err -> putStrLn err
  Right (m, e) -> compile e m
 where
  attempt = do
    e <- first MP.errorBundlePretty $ parseProgram src
    let (m, _) = runChecker $ synthToModule "test" e
    first (errorFinish src) m