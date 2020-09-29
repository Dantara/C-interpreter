module Language.Syntax.AST where

newtype AST = AST [GlobalDeclaration] deriving (Eq, Show)

data GlobalDeclaration
  = FunctionDeclaration Function
  | GlobalVariableDeclaration VariableDeclaration
  | GlobalExpr Expr
  deriving (Eq, Show)

data Variable = Variable {
    varName  :: Identifier
  , varType  :: Type
  , varValue :: Maybe Value
                         } deriving (Eq, Show)

newtype Expr = Expr UnaryExpr deriving (Eq, Show)

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

data OrExpr
  = OrExpr Value Value
  | OrValue Value
  deriving (Eq, Show)

-- Check whether we need type arguments or not
-- Maybe should use GADTs
data Type
  = TypeInt
  | TypeFloat
  | TypeString
  | TypeBool
  deriving (Eq, Show)

data Value
  = IntValue Int
  | FloatValue Float
  | StringValue String
  | BoolValue Bool
  deriving (Eq, Show)

newtype Identifier = Identifier String deriving (Eq, Show)

data Function = Function {
    funcName :: Identifier
  , funcType :: Type
  , funcArgs :: [Variable]
  , funcBody :: [LocalDeclaration]
                         } deriving (Eq, Show)

data LocalDeclaration
  = LocalVariableDeclaration VariableDeclaration
  | FunctionCall Identifier [Value]
  | LoopDeclation Loop
  | IfDeclaration If
  | LocalExpr Expr
  deriving (Eq, Show)

data If = If {
    ifCond   :: Expr
  , ifBody   :: [LocalDeclaration]
  , elseBody :: Maybe [LocalDeclaration]
             } deriving (Eq, Show)

data VariableDeclaration
  = VariableDeclaration Variable (Maybe Expr)
  | VariableAssignment VariableUpdate
  deriving (Eq, Show)

data VariableUpdate = VariableUpdate Identifier Expr deriving (Eq, Show)

data Loop
  = ForLoop For
  | WhileLoop While
  deriving (Eq, Show)

data For = For {
    forHeader :: ForHeader
  , forBody   :: [LocalDeclaration]
             } deriving (Eq, Show)

data While = While {
    whileHeader :: Expr
  , whileBody   :: [LocalDeclaration]
                   } deriving (Eq, Show)

data ForHeader = ForHeader {
    forVar  :: Maybe VariableDeclaration
  , forCond :: Maybe Expr
  , forUpd  :: Maybe VariableUpdate
                           } deriving (Eq, Show)
