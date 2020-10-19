module Language.PrettyPrinter.Types where


class ToSourceCode a where
  toSourceCode :: a -> String
