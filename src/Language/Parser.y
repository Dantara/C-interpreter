{

module Language.Parser where

import Language.Syntax.Token
import Language.Syntax.AST

}

%name parse
%tokentype { Token }
%error { parseError }

%token
  if { TokenIf }
  else { TokenElse }
  for { TokenFor }
  while { TokenWhile }
  return { TokenReturn }

  "," { TokenComma }
  ";" { TokenSemicolon }
  ")" { TokenOpenRoundBracket  }
  "(" { TokenCloseRoundBracket  }
  "{" { TokenOpenCurlyBracket  }
  "}" { TokenCloseCurlyBracket  }

  "+" { TokenPlus }
  "-" { TokenMinus }
  "*" { TokenMultiply }
  "/" { TokenDivide }

  "&&" { TokenAnd }
  "||" { TokenOr }
  "!" { TokenNot }

  "==" { TokenEq }
  "!=" { TokenNotEq }
  ">" { TokenGreater }
  "<" { TokenLess }
  ">=" { TokenGreaterOrEq }
  "<=" { TokenLessOrEq }

  "=" { TokenAssignment }

  int { TokenTypeDeclaration TokenTypeInt }
  float { TokenTypeDeclaration TokenTypeFloat }
  bool { TokenTypeDeclaration TokenTypeBool }
  string { TokenTypeDeclaration TokenTypeString }

  name { TokenIndentifier $$ }

  intVal { TokenInt $$ }
  floatVal { TokenFloat $$ }
  strVal { TokenString $$ }
  boolVal { TokenBool $$ }

%%

Program : GDecs { AST $1 }

GDecs : GDec { [$1] }
      | GDec GDecs { $1 : $2 }

GDec : Func { FunctionDeclaration $1 }
     | VarDec { GlobalVariableDeclaration $1 }
     | Expr { GlobalExpr $1 }

Func : Type Id "(" FArgs ")" "{" LDecs "}" { Function $2 $1 $4 $7 }

Type : int { TypeInt }
     | float { TypeFloat }
     | bool { TypeBool }
     | string { TypeString }

FArgs : {- empty -} { [] }
      | FArg "," FArgs { $1 : $3 }

FArg : Type Id { Variable $2 $1 Nothing }

VarDec : Type Id { VariableDeclaration (Variable $2 $1 Nothing) Nothing }
    | Type Id "=" Expr { VariableDeclaration (Variable $2 $1 Nothing) (Just $4) }
    | Id "=" Expr { VariableAssignment (VariableUpdate $1 $3) }

Expr : UnaryExpr { Expr $1 }
     | "(" UnaryExpr ")" {Expr $2 }

UnaryExpr : "+" "(" MultExpr ")" { UnaryExpr UnaryPositive $3 }
          | "-" "(" MultExpr ")" { UnaryExpr UnaryNegative $3 }
          | "+" MultExpr { UnaryExpr UnaryPositive $2 }
          | "-" MultExpr { UnaryExpr UnaryNegative $2 }
          | "(" MultExpr ")" { UnaryExpr UnaryPositive $2 }
          | Value { UnaryValue $1 }

MultExpr : "(" AddExpr ")" "*" "(" AddExpr ")" { MultExpr Multiply $2 $6 }
         | AddExpr "*" "(" AddExpr ")" { MultExpr Multiply $1 $4 }
         | "(" AddExpr ")" "*" AddExpr { MultExpr Multiply $2 $5 }
         | AddExpr "*" AddExpr { MultExpr Multiply $1 $3 }

         | "(" AddExpr ")" "/" "(" AddExpr ")" { MultExpr Divide $2 $6 }
         | AddExpr "/" "(" AddExpr ")" { MultExpr Divide $1 $4 }
         | "(" AddExpr ")" "/" AddExpr { MultExpr Divide $2 $5 }
         | AddExpr "/" AddExpr { MultExpr Divide $1 $3 }

         | Value { MultValue $1 }

AddExpr : "(" EqExpr ")" "+" "(" EqExpr ")" { AddExpr Addition $2 $6 }
        | EqExpr "+" "(" EqExpr ")" { AddExpr Addition $1 $4 }
        | "(" EqExpr ")" "+" EqExpr { AddExpr Addition $2 $5 }
        | EqExpr "+" EqExpr { AddExpr Addition $1 $3 }

        | "(" EqExpr ")" "-" "(" EqExpr ")" { AddExpr Substraction $2 $6 }
        | EqExpr "-" "(" EqExpr ")" { AddExpr Substraction $1 $4 }
        | "(" EqExpr ")" "-" EqExpr { AddExpr Substraction $2 $5 }
        | EqExpr "-" EqExpr { AddExpr Substraction $1 $3 }

        | Value { AddValue $1 }

EqExpr : "(" AndExpr ")" "==" "(" AndExpr ")" { EqExpr Equality $2 $6 }
       | AndExpr "==" "(" AndExpr ")" { EqExpr Equality $1 $4 }
       | "(" AndExpr ")" "==" AndExpr { EqExpr Equality $2 $5 }
       | AndExpr "==" AndExpr { EqExpr Equality $1 $3 }

       | "(" AndExpr ")" "!=" "(" AndExpr ")" { EqExpr Inequality $2 $6 }
       | AndExpr "!=" "(" AndExpr ")" { EqExpr Inequality $1 $4 }
       | "(" AndExpr ")" "!=" AndExpr { EqExpr Inequality $2 $5 }
       | AndExpr "!=" AndExpr { EqExpr Inequality $1 $3 }

       | Value { EqValue $1 }

AndExpr : "(" OrExpr ")" "&&" "(" OrExpr ")" { AndExpr $2 $6 }
        | OrExpr "&&" "(" OrExpr ")" { AndExpr $1 $4 }
        | "(" OrExpr ")" "&&" OrExpr { AndExpr $2 $5 }
        | OrExpr "&&" OrExpr { AndExpr $1 $3 }
        | Value { AndValue $1 }

OrExpr : "(" Value ")" "||" "(" Value ")" { OrExpr $2 $6 }
       | Value "||" "(" Value ")" { OrExpr $1 $4 }
       | "(" Value ")" "||" Value { OrExpr $2 $5 }
       | Value "||" Value { OrExpr $1 $3 }
       | Value { OrValue $1 }

Value : intVal { IntValue $1 }
      | floatVal { FloatValue $1 }
      | strVal { StringValue $1 }
      | boolVal { BoolValue $1 }

LDecs : {- empty -} { [] }
      | LDec LDecs { $1 : $2 }

LDec : VarDec { LocalVariableDeclaration $1 }
     | FuncCall { $1 }
     | LoopDec { LoopDeclation $1 }
     | IfDec { IfDeclaration $1 }
     | Expr { LocalExpr $1 }
     | Return { Return $1 }

FuncCall : Id "(" FParams ")" { FunctionCall $1 $3 }

FParams : {- empty -} { [] }
      | FParam "," FParams { $1 : $3 }

FParam : Expr { ExprParam $1 }
       | Id { VariableParam $1 }

LoopDec : for "(" ForVar ";" Cond ";" ForUpd ")" "{" LDecs "}"
          { ForLoop (For (ForHeader $3 $5 $7) $10) }
        | while "(" Cond ")" "{" LDecs "}" { WhileLoop (While $3 $6) }

ForVar : {- empty -} { Nothing }
       | VarDec { Just $1 }

Cond : {- empty -} { Nothing }
     | Expr { Just $1 }

ForUpd : {- empty -} { Nothing }
       | Id "=" Expr { Just (VariableUpdate $1 $3) }

IfDec : if "(" Cond ")" "{" LDecs "}" else "{" LDecs "}" { If $3 $6 $10 }
      | if "(" Cond ")" "{" LDecs "}" { If $3 $6 [] }

Return : return Id { ReturnVariable $2 }
       | return Expr { ReturnExpr $2 }

Id : name { Identifier $1 }

{

parseError :: [Token] -> a
parseError _ = error "Syntax error"

}
