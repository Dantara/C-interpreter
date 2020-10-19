module Main where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import qualified Data.Map.Strict                as Map
import           Language.Interpreter           (runUnwrapInterpreter)
import           Language.Interpreter.Internals
import           Language.Interpreter.Types
import           Language.Lexer
import           Language.Parser
import           Language.PrettyPrinter
import           Options.Applicative


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
      result <- runExceptT $ evalStateT (runApp $ runUnwrapInterpreter ast) initState
      either printErr (pure $ pure ()) result

    (Right ast, PrettyPrinter target) ->
      writeFile target (showSource ast)

    where
      printErr e = putStrLn $ "\nERROR OCCURED: " <> showInterpreterError e
