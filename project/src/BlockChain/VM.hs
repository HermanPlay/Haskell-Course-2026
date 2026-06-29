module BlockChain.VM
  ( Eval,
    EvalState (..),
    TxResult (..),

    -- * Running transactions
    runTransaction,
    initStateStore,

    -- * Low-level operations (used in tests)
    evalExpr,
    execStmt,
    runEvalPure,
  )
where

import BlockChain.Error (VMError (..))
import BlockChain.Syntax
import BlockChain.Value (Address, Store, TxEnv (..), Value (..), mkVMap)
import Data.Map.Strict qualified as Map

data EvalState = EvalState
  { esStore :: !Store,
    esEnv :: !TxEnv
  }

newtype Eval a = Eval {runEval :: EvalState -> Either VMError (a, EvalState)}

instance Functor Eval where
  fmap f (Eval p) = Eval $ \s ->
    case p s of
      Left err -> Left err
      Right (a, s') -> Right (f a, s')

instance Applicative Eval where
  pure a = Eval (\s -> Right (a, s))
  Eval pf <*> Eval px = Eval $ \s ->
    case pf s of
      Left err -> Left err
      Right (f, s') -> case px s' of
        Left err -> Left err
        Right (x, s'') -> Right (f x, s'')

instance Monad Eval where
  Eval p >>= f = Eval $ \s ->
    case p s of
      Left err -> Left err
      Right (a, s') -> runEval (f a) s'

------------------------------------------------------------
-- Primitive operations in the monad
------------------------------------------------------------

throwVM :: VMError -> Eval a
throwVM e = Eval (\_ -> Left e)

getEnv :: Eval TxEnv
getEnv = Eval (\s -> Right (esEnv s, s))

getStore :: Eval Store
getStore = Eval (\s -> Right (esStore s, s))

modifyStore :: (Store -> Store) -> Eval ()
modifyStore f = Eval (\s -> Right ((), s {esStore = f (esStore s)}))

------------------------------------------------------------
-- Variable lookup: params take precedence, then state vars
------------------------------------------------------------

lookupVar :: String -> Eval Value
lookupVar n = do
  env <- getEnv
  case Map.lookup n (txeParams env) of
    Just v -> pure v
    Nothing -> do
      st <- getStore
      case Map.lookup n st of
        Just v -> pure v
        Nothing -> throwVM (VMUnboundVar n)

------------------------------------------------------------
-- Default values (used when initialising map state vars)
------------------------------------------------------------

-- | The default value for a type (for missing-key map reads).
defaultValueFor :: Type -> Value
defaultValueFor = \case
  TInt -> VInt 0
  TBool -> VBool False
  TAddress -> VAddr "<zero-address>"
  TMap _ v -> defaultValueFor v

------------------------------------------------------------
-- Evaluating expressions
------------------------------------------------------------

evalExpr :: Expr -> Eval Value
evalExpr = \case
  EVar n -> lookupVar n
  ELit (LInt i) -> pure (VInt i)
  ELit (LBool b) -> pure (VBool b)
  ELit LEmpty ->
    throwVM
      (VMTypeError "empty map literal used as a runtime expression")
  ESender -> do
    env <- getEnv
    case txeSender env of
      Just a -> pure (VAddr a)
      Nothing -> throwVM VMSenderMissing
  EUnary op e -> do v <- evalExpr e; unaryOp op v
  EBinOp op a b -> do
    va <- evalExpr a
    vb <- evalExpr b
    binOp op va vb
  EIndex m k -> do
    mv <- evalExpr m
    kv <- evalExpr k
    case mv of
      VMap def inner ->
        pure (Map.findWithDefault def kv inner)
      _ -> throwVM (VMNotAMap (show mv))

------------------------------------------------------------
-- Operator semantics
------------------------------------------------------------

unaryOp :: UOp -> Value -> Eval Value
unaryOp UNeg (VInt i) = pure (VInt (negate i))
unaryOp UNeg v = throwVM (VMTypeError ("unary - on non-int: " ++ show v))
unaryOp UNot (VBool b) = pure (VBool (not b))
unaryOp UNot v = throwVM (VMTypeError ("unary ! on non-bool: " ++ show v))

requiresIntPair :: Value -> Value -> Eval (Int, Int)
requiresIntPair (VInt x) (VInt y) = pure (x, y)
requiresIntPair va vb =
  throwVM
    ( VMTypeError
        ( "arithmetic/comparison on non-int: "
            ++ show va
            ++ ", "
            ++ show vb
        )
    )

requiresBoolPair :: Value -> Value -> Eval (Bool, Bool)
requiresBoolPair (VBool x) (VBool y) = pure (x, y)
requiresBoolPair va vb =
  throwVM
    ( VMTypeError
        ( "logical op on non-bool: "
            ++ show va
            ++ ", "
            ++ show vb
        )
    )

binOp :: Op -> Value -> Value -> Eval Value
binOp op va vb = case op of
  OAdd -> do (x, y) <- ip; pure (VInt (x + y))
  OSub -> do (x, y) <- ip; pure (VInt (x - y))
  OMul -> do (x, y) <- ip; pure (VInt (x * y))
  ODiv -> do
    (x, y) <- ip
    if y == 0 then throwVM VMDivByZero else pure (VInt (x `div` y))
  OLt -> do (x, y) <- ip; pure (VBool (x < y))
  OGt -> do (x, y) <- ip; pure (VBool (x > y))
  OLe -> do (x, y) <- ip; pure (VBool (x <= y))
  OGe -> do (x, y) <- ip; pure (VBool (x >= y))
  OEq -> pure (VBool (va == vb))
  ONe -> pure (VBool (va /= vb))
  OAnd -> do (x, y) <- bp; pure (VBool (x && y))
  OOr -> do (x, y) <- bp; pure (VBool (x || y))
  where
    ip :: Eval (Int, Int)
    ip = requiresIntPair va vb
    bp :: Eval (Bool, Bool)
    bp = requiresBoolPair va vb

------------------------------------------------------------
-- Executing statements
------------------------------------------------------------

execStmt :: Stmt -> Eval ()
execStmt = \case
  SRequire e -> do
    v <- evalExpr e
    case v of
      VBool True -> pure ()
      VBool False -> throwVM (VMRevert (prettyExpr e))
      _ -> throwVM (VMTypeError "require on non-bool expression")
  SIf c ts fs -> do
    v <- evalExpr c
    case v of
      VBool True -> mapM_ execStmt ts
      VBool False -> mapM_ execStmt fs
      _ -> throwVM (VMTypeError "if on non-bool condition")
  SAssign lhs rhs -> do
    v <- evalExpr rhs
    assignL lhs v

-- | Assign to an l-value. Supported lvalues: state var or map[key].
-- Nested lvalues (e.g. map[a][b]) are NOT supported (type-checker permits them,
-- but assignL will raise a runtime error -- they are not needed for SimpleCoin).
assignL :: Expr -> Value -> Eval ()
assignL (EVar n) v = modifyStore (Map.insert n v)
assignL (EIndex m k) v = do
  mv <- evalExpr m
  kv <- evalExpr k
  case mv of
    VMap def inner ->
      case m of
        EVar varName ->
          modifyStore (Map.insert varName (VMap def (Map.insert kv v inner)))
        _ ->
          throwVM
            ( VMTypeError
                ("cannot assign through nested index: " ++ show m)
            )
    _ -> throwVM (VMNotAMap (show mv))
assignL other _ = throwVM (VMTypeError ("unsupported lvalue: " ++ show other))

------------------------------------------------------------
-- Initial state store from a contract
------------------------------------------------------------

-- | Build the initial store from a contract's state declarations.
-- The deployer address is used as @sender@ during state initialisation.
initStateStore :: Contract -> Address -> Either VMError Store
initStateStore (Contract _ stateVars _) deployer =
  go stateVars Map.empty
  where
    go [] st = Right st
    go (StateVar n t initExpr : rest) st =
      case initSV t initExpr st deployer of
        Left e -> Left e
        Right v -> do
          let st' = Map.insert n v st
          go rest st'

-- | Compute one state-var's initial value, given the already-initialised state.
-- A @map<_,t> = empty@ state var gets a typed-default empty map. All other
-- initialisers are evaluated against the current partial store.
initSV :: Type -> Expr -> Store -> Address -> Either VMError Value
initSV t initExpr st deployer =
  case (t, initExpr) of
    (TMap _ vt, ELit LEmpty) ->
      Right (mkVMap (defaultValueFor vt) [])
    _ ->
      case runEval (evalExpr initExpr) (EvalState st env) of
        Right (v, _) -> Right v
        Left e -> Left e
      where
        env = TxEnv {txeSender = Just deployer, txeParams = Map.empty}

------------------------------------------------------------
-- Running a transaction (with atomic rollback semantics)
------------------------------------------------------------

-- | Outcome of executing a transaction.
data TxResult
  = -- | the new store after the transaction
    TxCommitted !Store
  | -- | the transaction failed; store unchanged
    TxReverted !VMError
  deriving stock (Eq, Show)

-- | Execute a transaction body against a snapshot of the store.
-- Rollback is implicit: if evaluation yields 'Left', the caller keeps the
-- original store (we never return a partial one).
runTransaction ::
  -- | the transaction definition
  TxDef ->
  -- | the actual arguments, by name
  [(String, Value)] ->
  -- | submitter (sender)
  Address ->
  -- | prior store (a snapshot the caller holds)
  Store ->
  TxResult
runTransaction tx args sender store0 =
  case runEval (mapM_ execStmt (txBody tx)) (EvalState store0 env) of
    Right ((), st) -> TxCommitted (esStore st)
    Left e -> TxReverted e
  where
    env =
      TxEnv
        { txeSender = Just sender,
          txeParams = Map.fromList args
        }

-- | Run an 'Eval' computation directly (used in tests).
runEvalPure :: Eval a -> EvalState -> Either VMError (a, EvalState)
runEvalPure = runEval
