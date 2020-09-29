module Language.Syntax.AST where

newtype AST = AST [GlobalDeclaration] deriving (Eq, Show)

data GlobalDeclaration
  = FunctionDeclaration Function
  | GlobalVariableDeclaration Variable (Maybe Expr)
  | GlobalVariableAssignment Identifier Expr
  deriving (Eq, Show)

data Variable = Variable Identifier Value deriving (Eq, Show)

data Expr = Expr UnaryExpr deriving (Eq, Show)

data UnaryExpr
  = UnaryExpr UnaryAction MultExpr
  | UnaryValue Value
  deriving (Eq, Show)

data UnaryAction
  = UnaryNegative
  | UnaryPositive
  deriving (Eq, Show)

data MultExpr
  = MultExpr MultAction AddExpr AddExpr
  | MultValue Value
  deriving (Eq, Show)

data MultAction
  = Multiply
  | Divide
  deriving (Eq, Show)

data AddExpr
  = AddExpr AddAction EqExpr EqExpr
  | AddValue Value
  deriving (Eq, Show)

data AddAction
  = Addition
  | Substraction
  deriving (Eq, Show)

data EqExpr
  = EqExpr EqAction AndExpr AndExpr
  | EqValue Value
  deriving (Eq, Show)

data EqAction
 = Equality
 | Inequality
  deriving (Eq, Show)

data AndExpr
  = AndExpr OrExpr OrExpr
  | AndValue Value
  deriving (Eq, Show)

newtype OrExpr = OrExpr Value deriving (Eq, Show)

data Type
  = TypeInt Int
  | TypeFloat Float
  | TypeString String
  | TypeBool Bool
  deriving (Eq, Show)

newtype Value = Value Type deriving (Eq, Show)
newtype Identifier = Identifier String deriving (Eq, Show)

data Function = Function {
    fHeader :: Header
  , fBody   :: [LocalDeclaration]
                         } deriving (Eq, Show)

data Header = Header {
    hType :: Type
  , hArgs :: [Variable]
                     } deriving (Eq, Show)

data LocalDeclaration
  = VariableDeclaration Variable
  | FunctionCall Identifier [Value]
  deriving (Eq, Show)
