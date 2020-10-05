module Main where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.Either
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Language.Lexer
import           Language.Parser
import           Language.Syntax.AST

file :: String
file = "code_samples/hello-world.c"

main :: IO ()
main = do
  sourceCode <- readFile file

  let tokens = scanTokens sourceCode

  case parseTokens tokens of
    Left s ->
      putStrLn s

    Right ast -> do
      let initState =
            AppState Map.empty Map.empty Map.empty Nothing Global
      result <- runExceptT $ evalStateT (runApp $ interpret ast) initState
      either putStrLn (pure $ pure ()) result
