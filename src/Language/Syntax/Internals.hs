module Language.Syntax.Internals where

class ToSourceCode a where
  toSourceCode :: a -> String
