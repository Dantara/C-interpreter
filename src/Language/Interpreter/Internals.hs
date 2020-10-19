{-# LANGUAGE FlexibleContexts #-}

module Language.Interpreter.Internals where

import           Language.Interpreter.Types
import           Language.PrettyPrinter


showInterpreterError :: InterpreterError -> String
showInterpreterError (FunctionIsNotDefined id')
  = "Function with name " <> showSource id' <> " is not defined."

showInterpreterError (FunctionAlreadyDefined id')
  = "Function with name " <> showSource id' <> " was already defined."

showInterpreterError (ReservedIdentifier id')
  = "Identifier " <> showSource id' <> " is reserved."

showInterpreterError (TypeCastingError s)
  = "Type casting error at: " <> s

showInterpreterError (UnknownVariable id')
  = "Unknown variable: " <> showSource id'

showInterpreterError (UnknownFunction id')
  = "Unknown function: " <> showSource id'

showInterpreterError (WrongNumberOfArgs fn)
  = "Wrong number of arguments for the following function: "
  <> showSource fn

showInterpreterError (NoReturnStatement id')
  = "Function " <> showSource id' <> "has no return statement."

showInterpreterError (EmptyVariable id')
  = "Variable " <> showSource id' <> " has no value."
