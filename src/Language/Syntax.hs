{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Syntax where

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

  -- Boolean operators
  | TokenAnd
  | TokenOr
  | TokenNot
  | TokenXor
  | TokenEq

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
