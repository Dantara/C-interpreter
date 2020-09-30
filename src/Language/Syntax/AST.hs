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
  = UnaryExpr UnaryAction UnaryExpr
  | UnaryRawExpr MultExpr
  deriving (Eq, Show)

data UnaryAction
  = UnaryPlus
  | UnaryMinus
  | UnaryNot
  deriving (Eq, Show)

data MultExpr
  = MultExpr MultAction AddExpr MultExpr
  | MultRawExpr AddExpr
  deriving (Eq, Show)

data MultAction
  = Multiply
  | Divide
  deriving (Eq, Show)

data AddExpr
  = AddExpr AddAction RelationExpr AddExpr
  | AddRawExpr RelationExpr
  deriving (Eq, Show)

data AddAction
  = Addition
  | Substraction
  deriving (Eq, Show)

data RelationExpr
  = RelationExpr RelationAction EqExpr RelationExpr
  | RelationRawExpr EqExpr
  deriving (Eq, Show)

data RelationAction
  = Greater
  | Less
  | GreaterOrEq
  | LessOrEq
  deriving (Eq, Show)

data EqExpr
  = EqExpr EqAction AndExpr EqExpr
  | EqRawExpr AndExpr
  deriving (Eq, Show)

data EqAction
  = Equality
  | Inequality
  deriving (Eq, Show)

data AndExpr
  = AndExpr OrExpr AndExpr
  | AndRawExpr OrExpr
  deriving (Eq, Show)

data OrExpr
  = OrExpr BaseExpr OrExpr
  | OrRawExpr BaseExpr
  deriving (Eq, Show)

data BaseExpr
  = ValueExpr Value
  | VarExpr Identifier
  | FunctionCall Identifier [Expr]
  deriving (Eq, Show)

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
  | LoopDeclation Loop
  | IfDeclaration If
  | LocalExpr Expr
  | ReturnCall Return
  deriving (Eq, Show)

newtype Return = Return Expr deriving (Eq, Show)

data If = If {
    ifCond   :: Maybe Expr
  , ifBody   :: [LocalDeclaration]
  , elseBody :: [LocalDeclaration]
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

data ForHeader = ForHeader {
    forVar  :: Maybe VariableDeclaration
  , forCond :: Maybe Expr
  , forUpd  :: Maybe VariableUpdate
                           } deriving (Eq, Show)

data While = While {
    whileHeader :: Maybe Expr
  , whileBody   :: [LocalDeclaration]
                   } deriving (Eq, Show)
