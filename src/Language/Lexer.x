{

{-# LANGUAGE NoMonomorphismRestriction          #-}

module Language.Lexer (scanTokens) where

import Language.Syntax

}

%wrapper "basic"

$digit = 0-9
$letter = [a-zA-Z]

@string = \" [$digit $letter \_ \']+ \"
@float = $digit+ [ \. ] $digit*
@int = $digit+
$eol = [\n]

tokens :-

  $eol ;
  $white ;
  "//".* ;

  int { tok (TokenTypeDeclaration TokenTypeInt) }
  float { tok (TokenTypeDeclaration TokenTypeFloat) }
  bool { tok (TokenTypeDeclaration TokenTypeBool) }
  string { tok (TokenTypeDeclaration TokenTypeString) }

  if { tok TokenIf }
  else { tok TokenElse }
  for { tok TokenFor }
  while { tok TokenWhile }
  return { tok TokenReturn }

  \, { tok TokenComma }
  \; { tok TokenSemicolon }
  \( { tok TokenOpenRoundBracket }
  \) { tok TokenCloseRoundBracket }
  \{ { tok TokenOpenCurlyBracket }
  \} { tok TokenCloseCurlyBracket }

  [\+] { tok TokenPlus }
  [\-] { tok TokenMinus }
  [\*] { tok TokenMultiply }
  [\/] { tok TokenDivide }

  "&&" { tok TokenAnd }
  "||" { tok TokenOr }
  \! { tok TokenNot }
  \^ { tok TokenXor }

  "==" { tok TokenAssignment }
  \= { tok TokenEq }

  @float { tokValue TokenTypeFloat }
  @int { tokValue TokenTypeInt }

  0 { tokValue TokenTypeBool }
  1 { tokValue TokenTypeBool }

  @string { tokValue TokenTypeString }

  $letter [$letter $digit \_] { tokIdentifier }

{

tok :: Token -> String -> Token
tok t _ = t

tokValue :: TokenType -> String -> Token
tokValue TokenTypeString s = TokenString s
tokValue TokenTypeInt s = TokenInt (read s)
tokValue TokenTypeFloat s = TokenFloat (read s)
tokValue TokenTypeBool s
  | s == "0" = TokenBool False
  | otherwise = TokenBool True

tokIdentifier :: String -> Token
tokIdentifier s = TokenIndentifier s

scanTokens :: String -> [Token]
scanTokens = alexScanTokens

}
