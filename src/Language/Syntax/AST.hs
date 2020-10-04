{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Syntax.AST where

-- import           Control.Monad.Trans.Except
-- import           Control.Monad.Trans.State.Lazy
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Language.Syntax.Internals
import           Text.Read (readMaybe)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Lazy

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

newtype Identifier = Identifier String deriving (Eq, Show, Ord)

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



instance Castable Value where
  castToInt (IntValue x)   = IntValue x
  castToInt (FloatValue x) = IntValue $ round x
  castToInt (StringValue s) = IntValue $ mbToInt $ readMaybe s
    where
      mbToInt Nothing  = length s
      mbToInt (Just y) = y
  castToInt (BoolValue True) = IntValue 1
  castToInt (BoolValue False) = IntValue 0

  castToFloat (IntValue x)   = IntValue $ fromIntegral x
  castToFloat (FloatValue x) = FloatValue x
  castToFloat (StringValue s) = FloatValue $ mbToInt $ readMaybe s
    where
      mbToInt Nothing  = fromIntegral $ length s
      mbToInt (Just y) = y
  castToFloat (BoolValue True)  = FloatValue 1
  castToFloat (BoolValue False) = FloatValue 0

  castToString (IntValue x)      = StringValue $ show x
  castToString (FloatValue x)    = StringValue $ show x
  castToString (StringValue s)   = StringValue s
  castToString (BoolValue True)  = StringValue "1"
  castToString (BoolValue False) = StringValue ""

  castToBool (IntValue x)
    | x > 0 = BoolValue True
    | otherwise = BoolValue False
  castToBool (FloatValue x)
    | x > 0 = BoolValue True
    | otherwise = BoolValue False
  castToBool (StringValue s)
    | s == "" = BoolValue False
    | otherwise = BoolValue True
  castToBool (BoolValue x) = BoolValue x

data AppState = AppState {
    funcs      :: Map Identifier Function
  , globalVars :: Map Identifier Variable
  , localVars  :: Map Identifier Variable
                         }

newtype AppM a = AppM {
  runApp :: StateT AppState (ExceptT String IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError String
             , MonadState AppState
             )


class Interpretable a b | a -> b where
  interpret :: a -> AppM b
 
instance Interpretable Expr Value where
  interpret (Expr e) = interpret e

-- Figure out how to use bind
instance Interpretable UnaryExpr Value where
  interpret (UnaryExpr UnaryPlus e) = interpret e
  interpret (UnaryExpr UnaryMinus e) =
    interpret e >>= (handle . castToFloat)
    where
      handle (FloatValue x) = pure $ FloatValue $ (-1) * x
      handle x = throwError $ "Type casting error at: " <> toSourceCode x

  interpret (UnaryExpr UnaryNot e) =
    interpret e >>= (handle . castToFloat)
    where
      handle (BoolValue x) = pure $ BoolValue $ not x
      handle x = throwError $ "Type casting error at: " <> toSourceCode x

  interpret (UnaryRawExpr e) = interpret e
 

instance Interpretable MultExpr Value where
  interpret e@(MultExpr Multiply l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ FloatValue $ x * y
      handle _ _ = throwError $ "Type casting error at: " <> toSourceCode e

  interpret e@(MultExpr Divide l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ FloatValue $ x / y
      handle _ _ = throwError $ "Type casting error at: " <> toSourceCode e

  interpret (MultRawExpr e) = interpret e


instance Interpretable AddExpr Value where
  interpret e@(AddExpr Addition l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ FloatValue $ x + y
      handle _ _ = throwError $ "Type casting error at: " <> toSourceCode e

  interpret e@(AddExpr Substraction l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ FloatValue $ x - y
      handle _ _ = throwError $ "Type casting error at: " <> toSourceCode e

  interpret (AddRawExpr e) = interpret e


instance Interpretable RelationExpr Value where
  interpret e@(RelationExpr Greater l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ BoolValue $ x > y
      handle _ _ = throwError $ "Type casting error at: " <> toSourceCode e

  interpret e@(RelationExpr Less l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ BoolValue $ x < y
      handle _ _ = throwError $ "Type casting error at: " <> toSourceCode e

  interpret e@(RelationExpr GreaterOrEq l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ BoolValue $ x >= y
      handle _ _ = throwError $ "Type casting error at: " <> toSourceCode e

  interpret e@(RelationExpr LessOrEq l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ BoolValue $ x <= y
      handle _ _ = throwError $ "Type casting error at: " <> toSourceCode e

  interpret (RelationRawExpr e) = interpret e


instance Interpretable EqExpr Value where
  interpret (EqExpr Equality l r) = do
    l' <- interpret l
    r' <- interpret r
    pure $ handle l' r'
    where
      handle (FloatValue x) y = BoolValue $ x == toFloat y
      handle (IntValue x) y = BoolValue $ x == toInt y
      handle (StringValue x) y = BoolValue $ x == toString y
      handle (BoolValue x) y = BoolValue $ x == toBool y
      toFloat y = (\(FloatValue x) -> x) $ castToFloat y
      toInt y = (\(IntValue x) -> x) $ castToInt y
      toString y = (\(StringValue x) -> x) $ castToString y
      toBool y = (\(BoolValue x) -> x) $ castToBool y

  interpret (EqExpr Inequality l r) = do
    l' <- interpret l
    r' <- interpret r
    pure $ handle l' r'
    where
      handle (FloatValue x) y = BoolValue $ x /= toFloat y
      handle (IntValue x) y = BoolValue $ x /= toInt y
      handle (StringValue x) y = BoolValue $ x /= toString y
      handle (BoolValue x) y = BoolValue $ x /= toBool y
      toFloat y = (\(FloatValue x) -> x) $ castToFloat y
      toInt y = (\(IntValue x) -> x) $ castToInt y
      toString y = (\(StringValue x) -> x) $ castToString y
      toBool y = (\(BoolValue x) -> x) $ castToBool y

  interpret (EqRawExpr e) = interpret e

 

instance Interpretable AndExpr Value where
  interpret e@(AndExpr l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (BoolValue x) (BoolValue y) = pure $ BoolValue $ x && y
      handle _ _ = throwError $ "Type casting error at: " <> toSourceCode e

  interpret (AndRawExpr e) = interpret e


instance Interpretable OrExpr Value where
  interpret e@(OrExpr l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (BoolValue x) (BoolValue y) = pure $ BoolValue $ x || y
      handle _ _ = throwError $ "Type casting error at: " <> toSourceCode e

  interpret (OrRawExpr e) = interpret e

 
instance Interpretable BaseExpr Value where
  interpret (ValueExpr v) = interpret v

  interpret (VarExpr id') = do
    appState <- get
    let gvs = globalVars appState
    let lvs = localVars appState
    case (Map.lookup id' lvs) <|> (Map.lookup id' gvs) of
      Nothing ->
         throwError $ "Unknown variable: " <> toSourceCode id'
      Just v ->
         interpret v

  interpret (FunctionCall id' params) = do
    appState <- get
    let fs = funcs appState
    case Map.lookup id' fs of
      Nothing ->
        throwError $ "Unknown function: " <> toSourceCode id'
      Just f ->
        if length (funcArgs f) /= length params then
          throwError
          $ "Wrong number of arguments for the following function: "
          <> toSourceCode f
        else do
          vars <- mapM (uncurry plugExprToVar)
                            $ zip params (funcArgs f)
          interpret (f {funcArgs = vars})


plugExprToVar :: Expr -> Variable -> AppM Variable
plugExprToVar expr var = do
  val <- interpret expr
  pure $ var {varValue = Just val}


instance Interpretable Variable Value where
  interpret (Variable _ TypeInt (Just v)) = castToInt <$> interpret v
  interpret (Variable _ TypeFloat (Just v)) = castToFloat <$> interpret v
  interpret (Variable _ TypeString (Just v)) = castToString <$> interpret v
  interpret (Variable _ TypeBool (Just v)) = castToBool <$> interpret v
  interpret v@(Variable _ _ Nothing) = throwError
    $ "Variable is has no value: \n"
    <> toSourceCode v
 
instance Interpretable Value Value where
  interpret v = pure v
