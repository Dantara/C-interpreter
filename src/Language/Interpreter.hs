{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Interpreter where


import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.Map                     (Map)
import qualified Data.Map.Strict              as Map
import           Language.Interpreter.Cast
import           Language.Interpreter.Types
import           Language.PrettyPrinter
import           Language.PrettyPrinter
import           Language.PrettyPrinter.Types
import           Language.Syntax.AST


newtype Interpreter a = Interpreter {
  unInterpreter :: a
} deriving (Functor)


runUnwrapInterpreter :: (Functor m, MonadSTD m, Interpretable (Interpreter a) (Interpreter b))
  => a -> AppM m b
runUnwrapInterpreter x = unInterpreter <$> (interpret $ Interpreter x)


runInterpreter :: (MonadSTD m, Interpretable (Interpreter a) (Interpreter b))
  => a -> AppM m (Interpreter b)
runInterpreter = interpret . Interpreter


instance Interpretable (Interpreter AST) (Interpreter ()) where
  interpret (Interpreter (AST ds)) = do
    mapM_ runInterpreter ds
    fs <- funcs <$> get
    case Map.lookup (Identifier "main") fs of
      Just f ->
        runInterpreter f >> pure (Interpreter ())
      Nothing ->
        throwError $ FunctionIsNotDefined (Identifier "main")


instance Interpretable (Interpreter GlobalDeclaration) (Interpreter ()) where
  interpret (Interpreter (FunctionDeclaration f)) = do
    let id' = funcName f
    fs <- funcs <$> get
    case (Map.member id' fs, Map.member id' buildInFunctions) of
      (False, False) -> do
        let fs' =  Map.insert id' f fs
        modify (\x -> x { funcs = fs' })
        pure $ Interpreter ()
      (True, _) ->
        throwError $ FunctionAlreadyDefined id'
      (False, _) ->
        throwError $ ReservedIdentifier id'

  interpret (Interpreter (GlobalVariableDeclaration v)) =
    modify (\x -> x { scope = Global }) >> runInterpreter v

  interpret (Interpreter (GlobalExpr e)) = runInterpreter e >> pure (Interpreter ())


instance Interpretable (Interpreter Expr) (Interpreter Value) where
  interpret (Interpreter (Expr e)) = runInterpreter e


instance Interpretable (Interpreter UnaryExpr) (Interpreter Value) where
  interpret (Interpreter (UnaryExpr UnaryPlus e)) = runInterpreter e
  interpret (Interpreter (UnaryExpr UnaryMinus e)) =
    runUnwrapInterpreter e >>= (handle . unCaster . castToFloat . Caster)
    where
      handle (FloatValue x) = pure $ Interpreter $ FloatValue $ (-1) * x
      handle _              = throwError $ TypeCastingError $ showSource e

  interpret (Interpreter (UnaryExpr UnaryNot e)) =
    runUnwrapInterpreter e >>= (handle . unCaster . castToFloat . Caster)
    where
      handle (BoolValue x) = pure $ Interpreter $ BoolValue $ not x
      handle _             = throwError $ TypeCastingError $ showSource e

  interpret (Interpreter (UnaryRawExpr e)) = runInterpreter e


instance Interpretable (Interpreter MultExpr) (Interpreter Value) where
  interpret (Interpreter e@(MultExpr Multiply l r)) = do
    l' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter l
    r' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ Interpreter $ FloatValue $ x * y
      handle _ _ = throwError $ TypeCastingError $ showSource e

  interpret (Interpreter e@(MultExpr Divide l r)) = do
    l' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter l
    r' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ Interpreter $ FloatValue $ x / y
      handle _ _ = throwError $ TypeCastingError $ showSource e

  interpret (Interpreter (MultRawExpr e)) = runInterpreter e


instance Interpretable (Interpreter AddExpr) (Interpreter Value) where
  interpret (Interpreter e@(AddExpr Addition l r)) = do
    l' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter l
    r' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ Interpreter $ FloatValue $ x + y
      handle _ _ = throwError $ TypeCastingError $ showSource e

  interpret (Interpreter e@(AddExpr Substraction l r)) = do
    l' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter l
    r' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ Interpreter $ FloatValue $ x - y
      handle _ _ = throwError $ TypeCastingError $ showSource e

  interpret (Interpreter (AddRawExpr e)) = runInterpreter e


instance Interpretable (Interpreter RelationExpr) (Interpreter Value) where
  interpret (Interpreter e@(RelationExpr Greater l r)) = do
    l' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter l
    r' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ Interpreter $ BoolValue $ x > y
      handle _ _ = throwError $ TypeCastingError $ showSource e

  interpret (Interpreter e@(RelationExpr Less l r)) = do
    l' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter l
    r' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ Interpreter $ BoolValue $ x < y
      handle _ _ = throwError $ TypeCastingError $ showSource e

  interpret (Interpreter e@(RelationExpr GreaterOrEq l r)) = do
    l' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter l
    r' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ Interpreter $ BoolValue $ x >= y
      handle _ _ = throwError $ TypeCastingError $ showSource e

  interpret (Interpreter e@(RelationExpr LessOrEq l r)) = do
    l' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter l
    r' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ Interpreter $ BoolValue $ x <= y
      handle _ _ = throwError $ TypeCastingError $ showSource e

  interpret (Interpreter (RelationRawExpr e)) = runInterpreter e


instance Interpretable (Interpreter EqExpr) (Interpreter Value) where
  interpret (Interpreter (EqExpr Equality l r)) = do
    l' <- runUnwrapInterpreter l
    r' <- runUnwrapInterpreter r
    pure $ Interpreter $ handle l' r'
    where
      handle (FloatValue x) y  = BoolValue $ x == toFloat y
      handle (IntValue x) y    = BoolValue $ x == toInt y
      handle (StringValue x) y = BoolValue $ x == toString y
      handle (BoolValue x) y   = BoolValue $ x == toBool y
      toFloat y = (\(FloatValue x) -> x) $ unCaster $ castToFloat $ Caster y
      toInt y = (\(IntValue x) -> x) $ unCaster $ castToInt $ Caster y
      toString y = (\(StringValue x) -> x) $ unCaster $ castToString $ Caster y
      toBool y = (\(BoolValue x) -> x) $ unCaster $ castToBool $ Caster y

  interpret (Interpreter (EqExpr Inequality l r)) = do
    l' <- runUnwrapInterpreter l
    r' <- runUnwrapInterpreter r
    pure $ Interpreter $ handle l' r'
    where
      handle (FloatValue x) y  = BoolValue $ x /= toFloat y
      handle (IntValue x) y    = BoolValue $ x /= toInt y
      handle (StringValue x) y = BoolValue $ x /= toString y
      handle (BoolValue x) y   = BoolValue $ x /= toBool y
      toFloat y = (\(FloatValue x) -> x) $ unCaster $ castToFloat $ Caster y
      toInt y = (\(IntValue x) -> x) $ unCaster $ castToInt $ Caster y
      toString y = (\(StringValue x) -> x) $ unCaster $ castToString $ Caster y
      toBool y = (\(BoolValue x) -> x) $ unCaster $ castToBool $ Caster y

  interpret (Interpreter (EqRawExpr e)) = runInterpreter e



instance Interpretable (Interpreter AndExpr) (Interpreter Value) where
  interpret (Interpreter e@(AndExpr l r)) = do
    l' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter l
    r' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter r
    handle l' r'
    where
      handle (BoolValue x) (BoolValue y) = pure $ Interpreter $ BoolValue $ x && y
      handle _ _ = throwError $ TypeCastingError $ showSource e

  interpret (Interpreter (AndRawExpr e)) = runInterpreter e


instance Interpretable (Interpreter OrExpr) (Interpreter Value) where
  interpret (Interpreter e@(OrExpr l r)) = do
    l' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter l
    r' <- unCaster . castToFloat . Caster <$> runUnwrapInterpreter r
    handle l' r'
    where
      handle (BoolValue x) (BoolValue y) = pure $ Interpreter $ BoolValue $ x || y
      handle _ _ = throwError $ TypeCastingError $ showSource e

  interpret (Interpreter (OrRawExpr e)) = runInterpreter e


data SpecialFunction = PrintF
    | ScanF
    deriving (Eq, Show)

buildInFunctions :: Map Identifier SpecialFunction
buildInFunctions = Map.fromList [
    (Identifier "printf", PrintF)
  , (Identifier "scanf", ScanF)
  ]


instance Interpretable (Interpreter BaseExpr) (Interpreter Value) where
  interpret (Interpreter (ValueExpr v)) = runInterpreter v

  interpret (Interpreter (VarExpr id')) = do
    appState <- get
    let gvs = globalVars appState
    let lvs = localVars appState
    case Map.lookup id' lvs <|> Map.lookup id' gvs of
      Nothing ->
         throwError $ UnknownVariable id'
      Just v ->
         runInterpreter v

  interpret (Interpreter (FunctionCall id' params)) = do
    appState <- get
    let fs = funcs appState
    case (Map.lookup id' buildInFunctions, Map.lookup id' fs) of
      (Just PrintF, _) -> do
        str <- mconcat
          <$> mapM (fmap ((\(StringValue s) -> s) . unCaster . castToString . Caster) . runUnwrapInterpreter)
          params
        writeSTD str
        pure $ Interpreter $ IntValue $ length str

      (Just ScanF, _) ->
        Interpreter . StringValue <$> readSTD

      (Nothing, Nothing) ->
        throwError $ UnknownFunction id'

      (Nothing, Just f) ->
        if length (funcArgs f) /= length params then
          throwError $ WrongNumberOfArgs f
        else do
          vars <- mapM (uncurry plugExprToVar)
                  $ zip params (funcArgs f)
          runInterpreter (f {funcArgs = vars})

plugExprToVar :: MonadSTD m => Expr -> Variable -> AppM m Variable
plugExprToVar expr var = do
  val <- runUnwrapInterpreter expr
  pure $ var {varValue = Just val}


instance Interpretable (Interpreter Function) (Interpreter Value) where
  interpret (Interpreter f@(Function id' _ args ds)) = do
    baseF <- currentFunc <$> get
    modify (\x -> x { currentFunc = Just f, localVars = varsToMap args })
    r <- runUnwrapInterpreter ds
    modify (\x -> x { currentFunc = baseF, localVars = Map.empty })
    case r of
      Just v ->
        pure $ Interpreter v
      Nothing ->
        throwError $ NoReturnStatement id'
    where
      varsToMap :: [Variable] -> Map Identifier Variable
      varsToMap vars = Map.fromList $ zip (varName <$> vars) vars


instance Interpretable (Interpreter [LocalDeclaration]) (Interpreter (Maybe Value)) where
  interpret (Interpreter [])                   = pure $ Interpreter Nothing
  interpret (Interpreter (r@(ReturnCall _):_)) = runInterpreter r
  interpret (Interpreter (d:ds))               = runInterpreter d >> runInterpreter ds


instance Interpretable (Interpreter LocalDeclaration) (Interpreter (Maybe Value)) where
  interpret (Interpreter (ReturnCall r)) = Interpreter . Just <$> runUnwrapInterpreter r
  interpret (Interpreter (LocalExpr e)) = runInterpreter e >> pure (Interpreter Nothing)
  interpret (Interpreter (LocalVariableDeclaration v)) = do
    modify (\x -> x { scope = Local })
    runInterpreter v
    pure $ Interpreter Nothing
  interpret (Interpreter (IfDeclaration e)) = runInterpreter e
  interpret (Interpreter (LoopDeclaration l)) = runInterpreter l


instance Interpretable (Interpreter Return) (Interpreter Value) where
  interpret (Interpreter (Return e)) = runInterpreter e


instance Interpretable (Interpreter VariableDeclaration) (Interpreter ()) where
  interpret (Interpreter (VariableDeclaration var Nothing)) = do
    s <- scope <$> get
    case s of
      Local -> do
        modify addLocal
        pure $ Interpreter ()
      Global -> do
        modify addGlobal
        pure $ Interpreter ()
    where
      addLocal t = t {
        localVars = Map.singleton (varName var) var <> localVars t
        }
      addGlobal t = t {
        globalVars = Map.singleton (varName var) var <> globalVars t
        }

  interpret (Interpreter (VariableDeclaration var (Just e))) = do
    s <- scope <$> get
    var' <- plugExprToVar e var
    case s of
      Local -> do
        modify (\t -> t {
          localVars = Map.singleton (varName var) var' <> localVars t
        })
        pure $ Interpreter ()
      Global -> do
        modify (\t -> t {
          globalVars = Map.singleton (varName var) var' <> globalVars t
        })
        pure $ Interpreter ()

  interpret (Interpreter (VariableAssignment v)) = runInterpreter v


instance Interpretable (Interpreter If) (Interpreter (Maybe Value)) where
  interpret (Interpreter (If (Just cond) ib eb)) = do
    v' <- runUnwrapInterpreter cond
    let (BoolValue cond') = unCaster $ castToBool $ Caster v'
    if cond' then runInterpreter ib else runInterpreter eb

  interpret (Interpreter (If Nothing ib _)) = do
    runInterpreter ib


instance Interpretable (Interpreter Loop) (Interpreter (Maybe Value)) where
  interpret (Interpreter (WhileLoop l)) = runInterpreter l
  interpret (Interpreter (ForLoop l))   = runInterpreter l


instance Interpretable (Interpreter While) (Interpreter (Maybe Value)) where
  interpret (Interpreter l@(While Nothing b)) =
    runInterpreter b >> runInterpreter l

  interpret (Interpreter l@(While (Just cond) b)) = do
    v <- runUnwrapInterpreter cond
    b' <- runUnwrapInterpreter b
    case (unCaster $ castToBool $ Caster v, b') of
      (BoolValue False, _) ->
        pure $ Interpreter Nothing
      (BoolValue True, Nothing) ->
        runInterpreter l
      (BoolValue True, r) ->
        pure $ Interpreter r
      (_, _) ->
        throwError $ TypeCastingError $ showSource cond


instance Interpretable (Interpreter For) (Interpreter (Maybe Value)) where
  interpret (Interpreter (For (ForHeader v c vu) b)) = evalV v >> loop
    where
      evalV (Just v') = runInterpreter v' >> pure (Interpreter Nothing)
      evalV Nothing   = pure $ Interpreter Nothing
      evalC (Just c') = unCaster . castToBool . Caster <$> runUnwrapInterpreter c'
      evalC Nothing   = pure $ BoolValue True
      loop = do
        cond <- evalC c
        b' <- runUnwrapInterpreter b
        case (cond, b') of
          (BoolValue False, _) ->
            pure $ Interpreter Nothing
          (BoolValue True, Nothing) ->
            evalV vu >> loop
          (BoolValue True, r) ->
            pure $ Interpreter r
          (_, _) ->
            throwError $ TypeCastingError $ showSource cond


instance Interpretable (Interpreter VariableUpdate) (Interpreter ()) where
  interpret (Interpreter (VariableUpdate id' e)) = do
    locals <- localVars <$> get
    globals <- globalVars <$> get

    case (Map.lookup id' locals, Map.lookup id' globals) of
      (Just v, _) -> do
        v' <- plugExprToVar e v
        let updatedLocals = Map.update (\_ -> Just v') id' locals
        modify (\x -> x { localVars = updatedLocals })
        pure $ Interpreter ()
      (_, Just v) -> do
        v' <- plugExprToVar e v
        let updatedGlobals = Map.update (\_ -> Just v') id' globals
        modify (\x -> x { globalVars = updatedGlobals })
        pure $ Interpreter ()
      (_, _) ->
        throwError $ UnknownVariable id'


instance Interpretable (Interpreter Variable) (Interpreter Value) where
  interpret (Interpreter (Variable _ TypeInt (Just v))) =
    fmap (unCaster . castToInt . Caster) <$> runInterpreter v
  interpret (Interpreter (Variable _ TypeFloat (Just v))) =
    fmap (unCaster . castToFloat . Caster) <$> runInterpreter v
  interpret (Interpreter (Variable _ TypeString (Just v))) =
    fmap (unCaster . castToString . Caster) <$> runInterpreter v
  interpret (Interpreter (Variable _ TypeBool (Just v))) =
    fmap (unCaster . castToBool . Caster) <$> runInterpreter v
  interpret (Interpreter (Variable id' _ Nothing)) = throwError $ EmptyVariable id'

instance Interpretable (Interpreter Value) (Interpreter Value) where
  interpret v = pure v
