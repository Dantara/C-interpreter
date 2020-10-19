module Language.PrettyPrinterSpec where

import           Language.Lexer
import           Language.Parser
import           Language.PrettyPrinter
import           Test.Hspec


spec :: Spec
spec = do
  describe "Pretty-Printer specs" $ do

    it "Hello World" $ do
      f <- readFile "code_samples/hello-world.c"

      let (Right ast) = parseTokens (scanTokens f)

      showSource ast `shouldBe` "int main() {\n   printf(\"Hello world\");\n   return 0;\n}\n\n"


    it "Echo" $ do
      f <- readFile "code_samples/echo.c"

      let (Right ast) = parseTokens (scanTokens f)

      showSource ast `shouldBe` "int main() {\n   string str = scanf();\n   printf(str);\n   return 0;\n}\n\n"
