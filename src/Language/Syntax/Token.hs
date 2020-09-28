module Language.Syntax.Token where

data TokenType
  = TokenTypeInt
  | TokenTypeFloat
  | TokenTypeBool
  | TokenTypeString
  deriving (Eq, Show)

data Token
  -- KEYWORDS
  = TokenIf
  | TokenElse
  -- Loops
  | TokenFor
  | TokenWhile
  -- Return
  | TokenReturn


  -- DELIMITERS
  | TokenComma
  | TokenSemicolon
  -- Brackets
  | TokenOpenRoundBracket
  | TokenCloseRoundBracket
  | TokenOpenCurlyBracket
  | TokenCloseCurlyBracket
  | TokenDelimiter


  -- OPERATORS
  -- Arithmetic operators
  | TokenPlus
  | TokenMinus
  | TokenMultiply
  | TokenDivide

  -- Logical operators
  | TokenAnd
  | TokenOr
  | TokenNot

  -- RelationalOperators
  | TokenEq
  | TokenNotEq
  | TokenGreater
  | TokenLess
  | TokenGreaterOrEq
  | TokenLessOrEq


  -- Assignment
  | TokenAssignment


  -- TYPE
  | TokenTypeDeclaration TokenType


  -- IDENTIFIER
  | TokenIndentifier String


  -- VALUE
  | TokenString String
  | TokenInt Int
  | TokenFloat Float
  | TokenBool Bool


  -- EOF
  | TokenEOF
  deriving (Eq, Show)
