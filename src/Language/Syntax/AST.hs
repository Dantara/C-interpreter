module Language.Syntax.AST where

import           Language.Syntax.Internals

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
  | LoopDeclaration Loop
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



instance ToSourceCode AST where
  toSourceCode (AST gds) = foldMap (\gd -> toSourceCode gd <> "\n\n") gds

instance ToSourceCode GlobalDeclaration where
  toSourceCode (FunctionDeclaration f)       = toSourceCode f
  toSourceCode (GlobalVariableDeclaration v) = toSourceCode v <> ";"
  toSourceCode (GlobalExpr e)                = toSourceCode e <> ";"

instance ToSourceCode Function where
  toSourceCode (Function id' type' args body) = mconcat [
      toSourceCode id'
    , " "
    , toSourceCode type'
    , "("
    , printArgs args
    , ") {\n"
    , printLocalDecs body
    , "}"
                                                        ]
    where
      printArgs []     = ""
      printArgs [v]    = toSourceCode v
      printArgs (v:vs) = toSourceCode v <> ", " <> printArgs vs

instance ToSourceCode LocalDeclaration where
  toSourceCode (LocalVariableDeclaration v) = toSourceCode v <> ";"
  toSourceCode (LoopDeclaration l)          = toSourceCode l
  toSourceCode (IfDeclaration e)            = toSourceCode e
  toSourceCode (LocalExpr e)                = toSourceCode e <> ";"
  toSourceCode (ReturnCall r)               = toSourceCode r <> ";"

instance ToSourceCode Return where
  toSourceCode (Return e) = "return " <> toSourceCode e

instance ToSourceCode VariableDeclaration where
  toSourceCode (VariableDeclaration v Nothing) = toSourceCode v
  toSourceCode (VariableDeclaration v (Just e)) = toSourceCode v <> " = " <> toSourceCode e
  toSourceCode (VariableAssignment v) = toSourceCode v

instance ToSourceCode VariableUpdate where
  toSourceCode (VariableUpdate id' e) = toSourceCode id' <> " = " <> toSourceCode e

instance ToSourceCode Variable where
  toSourceCode (Variable id' type' _) = toSourceCode type' <> toSourceCode id'

instance ToSourceCode Type where
  toSourceCode TypeInt    = "int"
  toSourceCode TypeFloat  = "float"
  toSourceCode TypeString = "string"
  toSourceCode TypeBool   = "bool"

instance ToSourceCode Loop where
  toSourceCode (ForLoop l)   = toSourceCode l
  toSourceCode (WhileLoop l) = toSourceCode l

instance ToSourceCode For where
  toSourceCode (For h b) = toSourceCode h
    <> " {\n" <> printLocalDecs b <> "}"

instance ToSourceCode ForHeader where
  toSourceCode (ForHeader var cond upd) = "for("
    <> mbToStr var <> "; " <> mbToStr cond <> mbToStr upd <> ")"

instance ToSourceCode While where
  toSourceCode (While h b) = "while(" <> mbToStr h <> ") {\n" <> printLocalDecs b <> "}"

instance ToSourceCode If where
  toSourceCode (If h b eb) = "if(" <> mbToStr h <> ") {\n"
    <> printLocalDecs b <> "}" <> printElse eb
    where
      printElse [] = ""
      printElse ds = "\n else {" <> printLocalDecs ds <> "}"

instance ToSourceCode Expr where
  toSourceCode (Expr e) = toSourceCode e

instance ToSourceCode UnaryExpr where
  toSourceCode (UnaryExpr UnaryPlus e)  = "+(" <> toSourceCode e <> ")"
  toSourceCode (UnaryExpr UnaryMinus e) = "-(" <> toSourceCode e <> ")"
  toSourceCode (UnaryExpr UnaryNot e)   = "!(" <> toSourceCode e <> ")"
  toSourceCode (UnaryRawExpr e)         = toSourceCode e

instance ToSourceCode MultExpr where
  toSourceCode (MultExpr Multiply l r) = toSourceCode l <> " * " <> toSourceCode r
  toSourceCode (MultExpr Divide l r) = toSourceCode l <> " / " <> toSourceCode r
  toSourceCode (MultRawExpr e) = toSourceCode e

instance ToSourceCode AddExpr where
  toSourceCode (AddExpr Addition l r) = toSourceCode l <> " * " <> toSourceCode r
  toSourceCode (AddExpr Substraction l r) = toSourceCode l <> " / " <> toSourceCode r
  toSourceCode (AddRawExpr e) = toSourceCode e

instance ToSourceCode RelationExpr where
  toSourceCode (RelationExpr Greater l r) = toSourceCode l <> " > " <> toSourceCode r
  toSourceCode (RelationExpr Less l r) = toSourceCode l <> " < " <> toSourceCode r
  toSourceCode (RelationExpr GreaterOrEq l r) = toSourceCode l <> " >= " <> toSourceCode r
  toSourceCode (RelationExpr LessOrEq l r) = toSourceCode l <> " <= " <> toSourceCode r
  toSourceCode (RelationRawExpr e) = toSourceCode e

instance ToSourceCode EqExpr where
  toSourceCode (EqExpr Equality l r) = toSourceCode l <> " == " <> toSourceCode r
  toSourceCode (EqExpr Inequality l r) = toSourceCode l <> " != " <> toSourceCode r
  toSourceCode (EqRawExpr e) = toSourceCode e

instance ToSourceCode AndExpr where
  toSourceCode (AndExpr l r)  = toSourceCode l <> " && " <> toSourceCode r
  toSourceCode (AndRawExpr e) = toSourceCode e

instance ToSourceCode OrExpr where
  toSourceCode (OrExpr l r)  = toSourceCode l <> " || " <> toSourceCode r
  toSourceCode (OrRawExpr e) = toSourceCode e

instance ToSourceCode BaseExpr where
  toSourceCode (ValueExpr v) = toSourceCode v
  toSourceCode (VarExpr v) = toSourceCode v
  toSourceCode (FunctionCall id' params) = mconcat [
    toSourceCode id'
    , "("
    , printParams params
    , ")"
                                                   ]
    where
      printParams []     = []
      printParams [p]    = toSourceCode p
      printParams (p:ps) = toSourceCode p <> ", " <> printParams ps

instance ToSourceCode Value where
  toSourceCode (IntValue v)    = show v
  toSourceCode (FloatValue v)  = show v
  toSourceCode (StringValue v) = v
  toSourceCode (BoolValue v)   = show v

instance ToSourceCode Identifier where
  toSourceCode (Identifier s) = s

printLocalDecs :: [LocalDeclaration] -> String
printLocalDecs = foldMap (\d -> "   " <> toSourceCode d <> "\n")

mbToStr :: ToSourceCode a => Maybe a -> String
mbToStr Nothing  = ""
mbToStr (Just x) = toSourceCode x
