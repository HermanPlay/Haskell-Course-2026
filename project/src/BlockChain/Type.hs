module BlockChain.Type
  ( checkContract,
    TypeError (..),
  )
where

import BlockChain.Error (TypeError (..), posStart)
import BlockChain.Syntax
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

------------------------------------------------------------
-- Contexts
------------------------------------------------------------

-- | Typing context: variable name -> declared type.
type Ctx = Map String Type

------------------------------------------------------------
-- Errors
------------------------------------------------------------

typeErr :: String -> Either TypeError a
typeErr msg = Left (TypeError posStart msg)

ensureEq :: Type -> Type -> Either TypeError ()
ensureEq expected actual
  | expected == actual = Right ()
  | otherwise =
      typeErr
        ( "expected "
            ++ prettyType expected
            ++ ", got "
            ++ prettyType actual
        )

------------------------------------------------------------
-- Bidirectional checking: checkExpr / inferExpr
------------------------------------------------------------

-- | Infer the type of an expression.
inferExpr :: Ctx -> Expr -> Either TypeError Type
inferExpr ctx = \case
  EVar n ->
    maybe (typeErr ("unbound variable: " ++ n)) Right (Map.lookup n ctx)
  ELit (LInt _) -> pure TInt
  ELit (LBool _) -> pure TBool
  ELit LEmpty -> typeErr "empty map needs an expected type"
  EBinOp op a b -> do
    ta <- inferExpr ctx a
    tb <- inferExpr ctx b
    binOpResult op ta tb
  EUnary op a -> do
    ta <- inferExpr ctx a
    unaryOpResult op ta
  EIndex m k -> do
    mt <- inferExpr ctx m
    case mt of
      TMap kt vt -> do
        checkExpr ctx kt k
        pure vt
      _ -> typeErr "indexing a non-map expression"
  ESender -> pure TAddress

-- | Check an expression has an expected type (bidirectional).
checkExpr :: Ctx -> Type -> Expr -> Either TypeError ()
checkExpr ctx expected expr =
  case expr of
    ELit LEmpty ->
      case expected of
        TMap _ _ -> pure ()
        _ -> typeErr ("empty map expects map type, got " ++ prettyType expected)
    _ -> do
      t <- inferExpr ctx expr
      ensureEq expected t

------------------------------------------------------------
-- Operator typing
------------------------------------------------------------

-- | Compute result type of a binary operator, validating operand types.
binOpResult :: Op -> Type -> Type -> Either TypeError Type
binOpResult op ta tb = case op of
  OAdd -> arith
  OSub -> arith
  OMul -> arith
  ODiv -> arith
  OLt -> comp
  OGt -> comp
  OLe -> comp
  OGe -> comp
  OAnd -> logic
  OOr -> logic
  OEq -> eq
  ONe -> eq
  where
    arith
      | ta == TInt && tb == TInt = pure TInt
      | otherwise =
          typeErr
            ( "arithmetic expects int operands, got "
                ++ prettyType ta
                ++ ", "
                ++ prettyType tb
            )
    comp
      | ta == TInt && tb == TInt = pure TBool
      | otherwise =
          typeErr
            ( "comparison expects int operands, got "
                ++ prettyType ta
                ++ ", "
                ++ prettyType tb
            )
    logic
      | ta == TBool && tb == TBool = pure TBool
      | otherwise =
          typeErr
            ( "logical expects bool operands, got "
                ++ prettyType ta
                ++ ", "
                ++ prettyType tb
            )
    eq
      | ta == tb && (ta == TInt || ta == TBool || ta == TAddress) =
          pure TBool
      | otherwise =
          typeErr
            ( "== / != expect same int / bool / address operands, got "
                ++ prettyType ta
                ++ ", "
                ++ prettyType tb
            )

-- | Compute result type of a unary operator, validating operand type.
unaryOpResult :: UOp -> Type -> Either TypeError Type
unaryOpResult UNeg TInt = pure TInt
unaryOpResult UNeg t = typeErr ("unary - expects int, got " ++ prettyType t)
unaryOpResult UNot TBool = pure TBool
unaryOpResult UNot t = typeErr ("unary ! expects bool, got " ++ prettyType t)

------------------------------------------------------------
-- L-value (assignable) typing
------------------------------------------------------------

lvalType :: Ctx -> Expr -> Either TypeError Type
lvalType ctx = \case
  EVar n ->
    maybe (typeErr ("unbound variable: " ++ n)) Right (Map.lookup n ctx)
  EIndex m k -> do
    mt <- inferExpr ctx m
    case mt of
      TMap kt vt -> do
        checkExpr ctx kt k
        pure vt
      _ -> typeErr "assignment to a non-map index"
  _ -> typeErr "expression is not assignable"

------------------------------------------------------------
-- Statement checking
------------------------------------------------------------

checkStmt :: Ctx -> Stmt -> Either TypeError ()
checkStmt ctx = \case
  SAssign lhs rhs -> do
    lt <- lvalType ctx lhs
    checkExpr ctx lt rhs
  SRequire e -> checkExpr ctx TBool e
  SIf c ts fs -> do
    checkExpr ctx TBool c
    mapM_ (checkStmt ctx) ts
    mapM_ (checkStmt ctx) fs

------------------------------------------------------------
-- Contract checking
------------------------------------------------------------

-- | Type-check an entire contract. Returns 'Right ()' on success.
checkContract :: Contract -> Either TypeError ()
checkContract (Contract _ stateVars txDefs) = do
  baseCtx <- buildStateCtx stateVars
  mapM_ (checkTxDef baseCtx) txDefs
  where
    buildStateCtx :: [StateVar] -> Either TypeError Ctx
    buildStateCtx [] = pure Map.empty
    buildStateCtx ((StateVar n t initExpr : rest)) = do
      checkExpr Map.empty t initExpr -- state inits see only literals + sender
      restCtx <- buildStateCtx rest
      pure (Map.insert n t restCtx)

checkTxDef :: Ctx -> TxDef -> Either TypeError ()
checkTxDef baseCtx (TxDef _ params body) = do
  let paramCtx = Map.fromList params
  -- duplicate param names are simply overwritten; not flagged
  let ctx = Map.union baseCtx paramCtx
  mapM_ (checkStmt ctx) body
