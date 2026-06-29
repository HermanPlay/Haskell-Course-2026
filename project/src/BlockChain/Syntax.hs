module BlockChain.Syntax
  ( -- * Names
    Name,

    -- * Types
    Type (..),

    -- * Literals
    Literal (..),

    -- * Operators
    Op (..),
    UOp (..),

    -- * Expressions
    Expr (..),

    -- * Statements
    Stmt (..),

    -- * Declarations
    StateVar (..),
    TxDef (..),
    Contract (..),

    -- * Pretty-printing
    prettyContract,
    prettyType,
    prettyExpr,
    prettyStmt,
  )
where

-- | Identifier names are plain strings.
type Name = String

-- | Supported value types.
data Type
  = TInt
  | TBool
  | TAddress
  | TMap Type Type
  deriving stock (Eq, Show, Ord)

-- | Literal expressions (constants that appear in source).
data Literal
  = LInt Int
  | LBool Bool
  | -- | the empty map literal (initialiser for map state vars)
    LEmpty
  deriving stock (Eq, Show, Ord)

-- | Binary operators.
data Op
  = OAdd
  | OSub
  | OMul
  | ODiv -- arithmetic (int -> int -> int)
  | OLt
  | OGt
  | OLe
  | OGe -- comparison (int -> int -> bool)
  | OEq
  | ONe -- equality (works on int/bool/address)
  | OAnd
  | OOr -- logical (bool -> bool -> bool)
  deriving stock (Eq, Show, Ord)

-- | Unary operators.
data UOp
  = -- | arithmetic negation (int -> int)
    UNeg
  | -- | logical negation (bool -> bool)
    UNot
  deriving stock (Eq, Show, Ord)

-- | Expressions.
data Expr
  = EVar Name
  | ELit Literal
  | EBinOp Op Expr Expr
  | EUnary UOp Expr
  | -- | map[key]; only valid when first subexpr is a map
    EIndex Expr Expr
  | -- | address that submitted the current transaction
    ESender
  deriving stock (Eq, Show, Ord)

-- | Statements.
data Stmt
  = -- | lhs := rhs
    SAssign Expr Expr
  | -- | abort transaction (atomic rollback) unless bool
    SRequire Expr
  | -- | if cond then xs else ys
    SIf Expr [Stmt] [Stmt]
  deriving stock (Eq, Show, Ord)

-- | A state variable declaration.
data StateVar = StateVar
  { svName :: Name,
    svType :: Type,
    svInit :: Expr
  }
  deriving stock (Eq, Show, Ord)

-- | A transaction definition.
data TxDef = TxDef
  { txName :: Name,
    txParams :: [(Name, Type)],
    txBody :: [Stmt]
  }
  deriving stock (Eq, Show, Ord)

-- | A contract: state variables + transaction definitions.
data Contract = Contract
  { cName :: Name,
    cState :: [StateVar],
    cTxs :: [TxDef]
  }
  deriving stock (Eq, Show, Ord)

------------------------------------------------------------
-- Pretty-printer (used for round-trip property test)
------------------------------------------------------------

-- | Render a contract back to canonical source text.
prettyContract :: Contract -> String
prettyContract (Contract name st txs) =
  "contract "
    ++ name
    ++ " {\n"
    ++ "  state {\n"
    ++ concatMap prettyStateVar st
    ++ "  }\n"
    ++ concatMap prettyTxDef txs
    ++ "}\n"

prettyStateVar :: StateVar -> String
prettyStateVar (StateVar n t e) =
  "  " ++ n ++ ": " ++ prettyType t ++ " = " ++ prettyExpr e ++ ";\n"

prettyTxDef :: TxDef -> String
prettyTxDef (TxDef n ps body) =
  "  transaction "
    ++ n
    ++ "("
    ++ prettyParams ps
    ++ ") {\n"
    ++ concatMap prettyStmt body
    ++ "  }\n"
  where
    prettyParams [] = ""
    prettyParams [(p, t)] = p ++ ": " ++ prettyType t
    prettyParams ((p, t) : rest) = p ++ ": " ++ prettyType t ++ ", " ++ prettyParams rest

prettyType :: Type -> String
prettyType = \case
  TInt -> "int"
  TBool -> "bool"
  TAddress -> "address"
  TMap k v -> "map<" ++ prettyType k ++ ", " ++ prettyType v ++ ">"

prettyExpr :: Expr -> String
prettyExpr = \case
  EVar n -> n
  ELit l -> prettyLit l
  ESender -> "sender"
  EBinOp op a b -> "(" ++ prettyExpr a ++ " " ++ prettyOp op ++ " " ++ prettyExpr b ++ ")"
  EUnary op e -> "(" ++ prettyUOp op ++ prettyExpr e ++ ")"
  EIndex m k -> "(" ++ prettyExpr m ++ "[" ++ prettyExpr k ++ "])"

prettyLit :: Literal -> String
prettyLit = \case
  LInt i -> show i
  LBool b -> if b then "true" else "false"
  LEmpty -> "empty"

prettyOp :: Op -> String
prettyOp = \case
  OAdd -> "+"
  OSub -> "-"
  OMul -> "*"
  ODiv -> "/"
  OLt -> "<"
  OGt -> ">"
  OLe -> "<="
  OGe -> ">="
  OEq -> "=="
  ONe -> "!="
  OAnd -> "&&"
  OOr -> "||"

prettyUOp :: UOp -> String
prettyUOp UNeg = "-"
prettyUOp UNot = "!"

prettyStmt :: Stmt -> String
prettyStmt = \case
  SAssign lhs rhs -> "    " ++ prettyExpr lhs ++ " := " ++ prettyExpr rhs ++ ";\n"
  SRequire e -> "    require " ++ prettyExpr e ++ ";\n"
  SIf c ts fs ->
    "    if "
      ++ prettyExpr c
      ++ " {\n"
      ++ concatMap prettyStmt ts
      ++ "    } else {\n"
      ++ concatMap prettyStmt fs
      ++ "    }\n"
