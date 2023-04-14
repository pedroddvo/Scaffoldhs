module Checker.Monad where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState, State, evalState, gets, modify, runState)

import Checker.Context (Context, (<++))
import Checker.Context qualified as C
import Checker.Context qualified as Context
import Data.Bifunctor qualified as Bifunctor
import Data.Text (Text)
import Debug.Trace (traceM, traceShow, traceShowM)
import Error (Error (..), Pos)
import Name (Name (Name))
import Type (Existential, Type)
import Type qualified as T

data CheckerState = CheckerState
    { cs_ctx :: Context
    , cs_exist :: Existential
    }

data CheckerError
    = MismatchError
    | CheckerError Error

newtype Checker a = Checker {unChecker :: ExceptT CheckerError (State CheckerState) a}
    deriving (Functor, Applicative, Monad, MonadError CheckerError, MonadState CheckerState)

getCtx :: Checker Context
getCtx = gets cs_ctx

setCtx :: Context -> Checker ()
setCtx ctx = modify (\s -> s{cs_ctx = ctx})

modifyCtx :: (Context -> Context) -> Checker ()
modifyCtx f = do
    ctx <- getCtx
    setCtx (f ctx)

fresh :: Checker Existential
fresh = do
    modify (\s -> s{cs_exist = cs_exist s + 1})
    gets cs_exist

extend :: [Context.Elem] -> Checker ()
extend es = modifyCtx (<++ es)

withMarker :: Existential -> Checker a -> Checker a
withMarker alpha f = do
    extend [C.Marker alpha]
    b <- f
    modifyCtx (C.dropOn $ C.onMarker alpha)
    return b

withSplit :: Checker a -> Checker (a, Context)
withSplit f = do
    alpha <- fresh
    extend [C.Marker alpha]
    a <- f
    (ctx', ctx) <- C.splitOn (C.onMarker alpha) <$> getCtx
    setCtx ctx
    return (a, ctx')

withNewMarker :: Checker a -> Checker a
withNewMarker f = fresh >>= \e -> withMarker e f

errorLabel :: String -> Pos -> Checker a
errorLabel msg p = throwError . CheckerError $ Error msg p

runChecker :: Checker a -> (Either Error a, CheckerState)
runChecker c = case run c of
    (err, ctx) -> (Bifunctor.first toError err, ctx)
  where
    toError MismatchError = undefined
    toError (CheckerError e) = e
    run =
        flip runState (CheckerState C.builtins 0)
            . runExceptT
            . unChecker

freshName :: Text -> Checker Name
freshName name = Name.Name name . T.unExistential <$> fresh