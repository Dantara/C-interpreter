module Language.Utils where

import           Language.Syntax.Token

tokenClassToString :: TokenClass -> String
tokenClassToString TokenIf                                = "if"
tokenClassToString TokenElse                              = "else"
tokenClassToString TokenFor                               = "for"
tokenClassToString TokenWhile                             = "while"
tokenClassToString TokenReturn                            = "return"
tokenClassToString TokenComma                             = ","
tokenClassToString TokenSemicolon                         = ";"
tokenClassToString TokenOpenRoundBracket                  = "("
tokenClassToString TokenCloseRoundBracket                 = ")"
tokenClassToString TokenOpenCurlyBracket                  = "{"
tokenClassToString TokenCloseCurlyBracket                 = "}"
tokenClassToString TokenPlus                              = "+"
tokenClassToString TokenMinus                             = "-"
tokenClassToString TokenMultiply                          = "*"
tokenClassToString TokenDivide                            = "/"
tokenClassToString TokenAnd                               = "&&"
tokenClassToString TokenOr                                = "||"
tokenClassToString TokenNot                               = "!"
tokenClassToString TokenEq                                = "=="
tokenClassToString TokenNotEq                             = "!="
tokenClassToString TokenGreater                           = ">"
tokenClassToString TokenLess                              = "<"
tokenClassToString TokenGreaterOrEq                       = ">="
tokenClassToString TokenLessOrEq                          = "<="
tokenClassToString TokenAssignment                        = "=="
tokenClassToString (TokenTypeDeclaration TokenTypeInt)    = "int"
tokenClassToString (TokenTypeDeclaration TokenTypeFloat)  = "float"
tokenClassToString (TokenTypeDeclaration TokenTypeBool)   = "bool"
tokenClassToString (TokenTypeDeclaration TokenTypeString) = "int"
tokenClassToString (TokenIndentifier s)                   = s
tokenClassToString (TokenString s)                        = "\"" <> s <> "\""
tokenClassToString (TokenInt x)                           = show x
tokenClassToString (TokenFloat x)                         = show x
tokenClassToString (TokenBool x)                          = show x
tokenClassToString TokenEOF                               = ""
