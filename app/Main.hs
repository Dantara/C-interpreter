module Main where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.Either
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Language.Lexer
import           Language.Parser
import           Language.Syntax.AST
import           Language.Syntax.Internals
import           Options.Applicative

file :: String
file = "code_samples/hello-world.c"

data Mode
  = Interpreter
  | PrettyPrinter String

data AppConfig = AppConfig {
    sourceFile :: String
  , mode       :: Mode
  }

parseConfig :: Parser AppConfig
parseConfig = AppConfig
  <$> strArgument (metavar "SOURCE_FILE_NAME")
  <*> parseMode

parseMode :: Parser Mode
parseMode = flag Interpreter Interpreter (long "interpret" <> short 'i')
        <|> PrettyPrinter <$> strOption (  long "pretty-print"
                                        <> short 'p'
                                        <> metavar "TARGET_FILE_NAME"
                                        )

main :: IO ()
main = do
  execApp =<< execParser opts
  where
    opts = info (parseConfig <**> helper)
      ( fullDesc
     <> progDesc "Interpreter and pretty printer for C-like language"
      )


execApp :: AppConfig -> IO ()
execApp config = do
  sourceCode <- readFile $ sourceFile config

  let tokens = scanTokens sourceCode

  case (parseTokens tokens, mode config) of
    (Left s, _) ->
      putStrLn s

    (Right ast, Interpreter) -> do
      let initState =
            AppState Map.empty Map.empty Map.empty Nothing Global
      result <- runExceptT $ evalStateT (runApp $ interpret ast) initState
      either putStrLn (pure $ pure ()) result

    (Right ast, PrettyPrinter target) ->
      writeFile target (toSourceCode ast)
