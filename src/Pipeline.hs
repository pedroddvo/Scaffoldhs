module Pipeline where

import Checker.Context qualified as Context
import Checker.Infer qualified as Infer
import Checker.Monad (CheckerState, runChecker)
import Checker.Monad qualified as State
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace (traceM, traceShowM)
import Error (Error, errorFinish)
import Syntax.Ast qualified as Ast
import Syntax.Parser qualified as Parser
import Text.Megaparsec qualified as MP
import Type (Type)

parseProgram :: Text -> Either (MP.ParseErrorBundle Text Void) (Ast.Expr ())
parseProgram = MP.parse Parser.program ""

testParseProgram :: Text -> IO ()
testParseProgram src = case parseProgram src of
    Left err -> putStrLn $ MP.errorBundlePretty err
    Right e -> print e

synthExpr :: Ast.Expr () -> (Either Error (Type, Ast.Expr Type), CheckerState)
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
