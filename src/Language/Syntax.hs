module Language.Syntax where

data TokenType
  = TokenTypeInt
  | TokenTypeFloat
  | TokenTypeBool
  | TokenTypeString
  deriving (Eq, Show)

data TokenKeyword
  -- Conditions
  = TokenIf
  | TokenElse
  -- Loops
  | TokenFor
  | TokenWhile
  -- Return
  | TokenReturn
  deriving (Eq, Show)

data TokenDelimiter
  = TokenComma
  | TokenSemicolon
  -- Brackets
  | TokenOpenRoundBracket
  | TokenCloseRoundBracket
  | TokenOpenCurlyBracket
  | TokenCloseCurlyBracket
  deriving (Eq, Show)

data TokenArithmeticOperator
  = TokenPlus
  | TokenMinus
  | TokenMultiply
  | TokenDivide
  deriving (Eq, Show)

data TokenBoolOperator
  = TokenAnd
  | TokenOr
  | TokenNot
  | TokenXor
  deriving (Eq, Show)

data TokenOperator
  = TokenArithmeticOperator
  | TokenBoolOperator
  | TokenAssignment
  | TokenEq
  deriving (Eq, Show)

data TokenValue
  = TokenString String
  | TokenInt Int
  | TokenFloat Float
  | TokenBool Bool
  deriving (Eq, Show)

data Token
  -- Keyword
  = TokenKeyword
  -- Delimiter
  | TokenDelimiter
  -- Operator
  | TokenOperator
  -- Type
  | TokenTypeDeclaration TokenType
  -- Identifer
  | TokenIndentifier String
  -- Value
  | TokenValue
  -- End of the file
  | TokenEOF
  deriving (Eq, Show)
