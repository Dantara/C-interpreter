{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.PrettyPrinter where

import           Language.PrettyPrinter.Types
import           Language.Syntax.AST


newtype Sourcer a = Sourcer {
  unSourcer :: a
}


showSource :: ToSourceCode (Sourcer a) => a -> String
showSource = toSourceCode . Sourcer


instance ToSourceCode (Sourcer AST) where
  toSourceCode (Sourcer (AST gds)) = foldMap (\gd -> toSourceCode (Sourcer gd) <> "\n\n") gds


instance ToSourceCode (Sourcer GlobalDeclaration) where
  toSourceCode (Sourcer (FunctionDeclaration f))       = toSourceCode (Sourcer f)
  toSourceCode (Sourcer (GlobalVariableDeclaration v)) = toSourceCode (Sourcer v) <> ";"
  toSourceCode (Sourcer (GlobalExpr e))                = toSourceCode (Sourcer e) <> ";"


instance ToSourceCode (Sourcer Function) where
  toSourceCode (Sourcer (Function id' type' args body)) = mconcat [
      toSourceCode (Sourcer type')
    , " "
    , toSourceCode (Sourcer id')
    , "("
    , printArgs args
    , ") {\n"
    , printLocalDecs body
    , "}"
                                                        ]
    where
      printArgs []     = ""
      printArgs [v]    = toSourceCode (Sourcer v)
      printArgs (v:vs) = toSourceCode (Sourcer v) <> ", " <> printArgs vs


instance ToSourceCode (Sourcer LocalDeclaration) where
  toSourceCode (Sourcer (LocalVariableDeclaration v)) = toSourceCode (Sourcer v) <> ";"
  toSourceCode (Sourcer (LoopDeclaration l))          = toSourceCode (Sourcer l)
  toSourceCode (Sourcer (IfDeclaration e))            = toSourceCode (Sourcer e)
  toSourceCode (Sourcer (LocalExpr e))                = toSourceCode (Sourcer e) <> ";"
  toSourceCode (Sourcer (ReturnCall r))               = toSourceCode (Sourcer r) <> ";"


instance ToSourceCode (Sourcer Return) where
  toSourceCode (Sourcer (Return e)) = "return " <> toSourceCode (Sourcer e)


instance ToSourceCode (Sourcer VariableDeclaration) where
  toSourceCode (Sourcer (VariableDeclaration v Nothing)) = toSourceCode (Sourcer v)
  toSourceCode (Sourcer (VariableDeclaration v (Just e))) = toSourceCode (Sourcer v) <> " = " <> toSourceCode (Sourcer e)
  toSourceCode (Sourcer (VariableAssignment v)) = toSourceCode (Sourcer v)


instance ToSourceCode (Sourcer VariableUpdate) where
  toSourceCode (Sourcer (VariableUpdate id' e)) = toSourceCode (Sourcer id')
    <> " = " <> toSourceCode (Sourcer e)


instance ToSourceCode (Sourcer Variable) where
  toSourceCode (Sourcer (Variable id' t' _)) = toSourceCode (Sourcer t') <> " " <> toSourceCode (Sourcer id')


instance ToSourceCode (Sourcer Type) where
  toSourceCode (Sourcer TypeInt)    = "int"
  toSourceCode (Sourcer TypeFloat)  = "float"
  toSourceCode (Sourcer TypeString) = "string"
  toSourceCode (Sourcer TypeBool)   = "bool"


instance ToSourceCode (Sourcer Loop) where
  toSourceCode (Sourcer (ForLoop l))   = toSourceCode (Sourcer l)
  toSourceCode (Sourcer (WhileLoop l)) = toSourceCode (Sourcer l)


instance ToSourceCode (Sourcer For) where
  toSourceCode (Sourcer (For h b)) = toSourceCode (Sourcer h)
    <> " {\n" <> printLocalDecs b <> "}"


instance ToSourceCode (Sourcer ForHeader) where
  toSourceCode (Sourcer (ForHeader var cond upd)) = "for("
    <> mbToStr (Sourcer var) <> "; " <> mbToStr (Sourcer cond) <> mbToStr (Sourcer upd) <> ")"


instance ToSourceCode (Sourcer While) where
  toSourceCode (Sourcer (While h b)) = "while(" <> mbToStr (Sourcer h) <> ") {\n" <> printLocalDecs b <> "}"


instance ToSourceCode (Sourcer If) where
  toSourceCode (Sourcer (If h b eb)) = "if(" <> mbToStr (Sourcer h) <> ") {\n"
    <> printLocalDecs b <> "}" <> printElse eb
    where
      printElse [] = ""
      printElse ds = "\n else {" <> printLocalDecs ds <> "}"


instance ToSourceCode (Sourcer Expr) where
  toSourceCode (Sourcer (Expr e)) = toSourceCode (Sourcer e)


instance ToSourceCode (Sourcer UnaryExpr) where
  toSourceCode (Sourcer (UnaryExpr UnaryPlus e))  = "+(" <> toSourceCode (Sourcer e) <> ")"
  toSourceCode (Sourcer (UnaryExpr UnaryMinus e)) = "-(" <> toSourceCode (Sourcer e) <> ")"
  toSourceCode (Sourcer (UnaryExpr UnaryNot e))   = "!(" <> toSourceCode (Sourcer e) <> ")"
  toSourceCode (Sourcer (UnaryRawExpr e))         = toSourceCode (Sourcer e)


instance ToSourceCode (Sourcer MultExpr) where
  toSourceCode (Sourcer (MultExpr Multiply l r)) = toSourceCode (Sourcer l) <> " * " <> toSourceCode (Sourcer r)
  toSourceCode (Sourcer (MultExpr Divide l r)) = toSourceCode (Sourcer l) <> " / " <> toSourceCode (Sourcer r)
  toSourceCode (Sourcer (MultRawExpr e)) = toSourceCode (Sourcer e)


instance ToSourceCode (Sourcer AddExpr) where
  toSourceCode (Sourcer (AddExpr Addition l r)) = toSourceCode (Sourcer l) <> " * " <> toSourceCode (Sourcer r)
  toSourceCode (Sourcer (AddExpr Substraction l r)) = toSourceCode (Sourcer l) <> " / " <> toSourceCode (Sourcer r)
  toSourceCode (Sourcer (AddRawExpr e)) = toSourceCode (Sourcer e)


instance ToSourceCode (Sourcer RelationExpr) where
  toSourceCode (Sourcer (RelationExpr Greater l r)) = toSourceCode (Sourcer l) <> " > " <> toSourceCode (Sourcer r)
  toSourceCode (Sourcer (RelationExpr Less l r)) = toSourceCode (Sourcer l) <> " < " <> toSourceCode (Sourcer r)
  toSourceCode (Sourcer (RelationExpr GreaterOrEq l r)) = toSourceCode (Sourcer l) <> " >= " <> toSourceCode (Sourcer r)
  toSourceCode (Sourcer (RelationExpr LessOrEq l r)) = toSourceCode (Sourcer l) <> " <= " <> toSourceCode (Sourcer r)
  toSourceCode (Sourcer (RelationRawExpr e)) = toSourceCode (Sourcer e)


instance ToSourceCode (Sourcer EqExpr) where
  toSourceCode (Sourcer (EqExpr Equality l r)) = toSourceCode (Sourcer l)
        <> " == "
        <> toSourceCode (Sourcer r)
  toSourceCode (Sourcer (EqExpr Inequality l r)) = toSourceCode (Sourcer l)
        <> " != "
        <> toSourceCode (Sourcer r)
  toSourceCode (Sourcer (EqRawExpr e)) = toSourceCode (Sourcer e)


instance ToSourceCode (Sourcer AndExpr) where
  toSourceCode (Sourcer (AndExpr l r))  = toSourceCode (Sourcer l)
        <> " && " <> toSourceCode (Sourcer r)
  toSourceCode (Sourcer (AndRawExpr e)) = toSourceCode (Sourcer e)


instance ToSourceCode (Sourcer OrExpr) where
  toSourceCode (Sourcer (OrExpr l r))  = toSourceCode (Sourcer l)
        <> " || "
        <> toSourceCode (Sourcer r)
  toSourceCode (Sourcer (OrRawExpr e)) = toSourceCode (Sourcer e)


instance ToSourceCode (Sourcer BaseExpr) where
  toSourceCode (Sourcer (ValueExpr v)) = toSourceCode $ Sourcer v
  toSourceCode (Sourcer (VarExpr v)) = toSourceCode $ Sourcer v
  toSourceCode (Sourcer (FunctionCall id' params)) = mconcat [
    toSourceCode (Sourcer id')
    , "("
    , printParams params
    , ")"
                                                   ]
    where
      printParams []     = []
      printParams [p]    = toSourceCode (Sourcer p)
      printParams (p:ps) = toSourceCode (Sourcer p) <> ", " <> printParams ps


instance ToSourceCode (Sourcer Value) where
  toSourceCode (Sourcer (IntValue v))    = show v
  toSourceCode (Sourcer (FloatValue v))  = show v
  toSourceCode (Sourcer (StringValue v)) = v
  toSourceCode (Sourcer (BoolValue v))   = show v


instance ToSourceCode (Sourcer Identifier) where
  toSourceCode (Sourcer (Identifier s)) = s


printLocalDecs :: [LocalDeclaration] -> String
printLocalDecs = foldMap (\d -> "   " <> toSourceCode (Sourcer d) <> "\n")


mbToStr :: ToSourceCode (Sourcer a) => Sourcer (Maybe a) -> String
mbToStr (Sourcer Nothing)  = ""
mbToStr (Sourcer (Just x)) = toSourceCode (Sourcer x)
