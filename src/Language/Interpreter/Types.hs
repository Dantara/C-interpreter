{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Interpreter.Types where


import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.Map.Strict           (Map)
import           Language.Syntax.AST


class Castable a where
  castToInt :: a -> a
  castToFloat :: a -> a
  castToString :: a -> a
  castToBool :: a -> a


class Monad m => MonadSTD m where
  readSTD :: m String
  writeSTD :: String -> m ()


instance MonadSTD IO where
  readSTD = getLine
  writeSTD = putStrLn


data Scope
  = Local
  | Global
  deriving (Eq, Show)

data AppState = AppState
    { funcs       :: Map Identifier Function
    , globalVars  :: Map Identifier Variable
    , localVars   :: Map Identifier Variable
    , currentFunc :: Maybe Function
    , scope       :: Scope
    }


data InterpreterError = FunctionIsNotDefined Identifier
  | FunctionAlreadyDefined Identifier
  | ReservedIdentifier Identifier
  | TypeCastingError String
  | UnknownVariable Identifier
  | UnknownFunction Identifier
  | WrongNumberOfArgs Function
  | NoReturnStatement Identifier
  | EmptyVariable Identifier
  deriving (Show, Eq)


newtype AppM m a = AppM {
  runApp :: StateT AppState (ExceptT InterpreterError m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError InterpreterError
             , MonadState AppState
             , MonadIO
             )


instance MonadTrans AppM where
  lift = AppM . lift . lift


instance MonadSTD m => MonadSTD (AppM m) where
  readSTD = lift readSTD
  writeSTD = lift . writeSTD


class Interpretable a b | a -> b where
  interpret :: MonadSTD m => a -> AppM m b
