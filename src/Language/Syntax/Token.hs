module Language.Syntax.Token where

data Token = Token TokenPosition TokenClass deriving (Eq, Show)

type TokenPosition = (Int, Int)

data TokenClass
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

  -- Relational operators
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

data TokenType
  = TokenTypeInt
  | TokenTypeFloat
  | TokenTypeBool
  | TokenTypeString
  deriving (Eq, Show)
