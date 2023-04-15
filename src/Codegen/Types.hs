module Codegen.Types where

import LLVM.AST qualified as LLVM
import Type (Type)
import Type qualified as T
import Unique qualified
import LLVM.AST.Type (i32)

returns :: Type -> Type
returns = \case
    T.Arrow _ b -> b
    _ -> undefined

args :: Type -> [Type]
args = reverse . tail . aux
  where
    aux =
        \case
            T.Arrow a b -> b : aux a
            t -> [t]

typeToLLVM :: Type -> LLVM.Type
typeToLLVM = \case
    T.Base (Unique.Builtin "Int") -> i32
    a@T.Arrow{} ->
        LLVM.FunctionType
            { LLVM.resultType = typeToLLVM $ returns a
            , LLVM.argumentTypes = map typeToLLVM (args a)
            , LLVM.isVarArg = False
            }