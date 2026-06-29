module BlockChain.Error
  ( -- * Source positions
    Pos (..),
    posStart,
    advancePos,

    -- * Parse errors
    ParseError (..),
    renderParseError,

    -- * Type-check errors
    TypeError (..),
    renderTypeError,

    -- * VM errors
    VMError (..),
    renderVMError,

    -- * Ledger / chain errors
    ChainError (..),
    renderChainError,

    -- * Re-export
    module BlockChain.Syntax,
  )
where

import BlockChain.Syntax

------------------------------------------------------------
-- Positions
------------------------------------------------------------

-- | 1-based source position.
data Pos = Pos {pLine, pCol :: !Int}
  deriving stock (Eq, Show, Ord)

-- | First position of a file.
posStart :: Pos
posStart = Pos 1 1

-- | Advance position by consuming a character.
advancePos :: Char -> Pos -> Pos
advancePos '\n' (Pos l _) = Pos (l + 1) 1
advancePos _ (Pos l c) = Pos l (c + 1)

------------------------------------------------------------
-- Parse errors
------------------------------------------------------------

data ParseError = ParseError
  { pePos :: Pos,
    peMsg :: String
  }
  deriving stock (Eq, Show)

renderParseError :: ParseError -> String
renderParseError (ParseError (Pos l c) msg) =
  "parse error at line " ++ show l ++ ", col " ++ show c ++ ": " ++ msg

------------------------------------------------------------
-- Type errors
------------------------------------------------------------

data TypeError = TypeError
  { tePos :: Pos,
    teMsg :: String
  }
  deriving stock (Eq, Show)

renderTypeError :: TypeError -> String
renderTypeError (TypeError (Pos l c) msg) =
  "type error at line " ++ show l ++ ", col " ++ show c ++ ": " ++ msg

------------------------------------------------------------
-- VM (runtime) errors
------------------------------------------------------------

data VMError
  = VMUnboundVar String
  | -- | e.g. "expected Int, got Bool"
    VMTypeError String
  | VMDivByZero
  | VMSenderMissing
  | -- | indexed something that wasn't a map
    VMNotAMap String
  | -- | a 'require' failed; carries the original expr text
    VMRevert String
  deriving stock (Eq, Show)

renderVMError :: VMError -> String
renderVMError = \case
  VMUnboundVar n -> "unbound variable: " ++ n
  VMTypeError s -> "type error: " ++ s
  VMDivByZero -> "division by zero"
  VMSenderMissing -> "no sender set in transaction environment"
  VMNotAMap s -> "not a map: " ++ s
  VMRevert s -> "require failed: " ++ s

------------------------------------------------------------
-- Chain errors
------------------------------------------------------------

data ChainError
  = -- | block named a parent that does not exist
    ChainParentUnknown Int
  | -- | non-genesis block had no parent
    ChainParentMissing
  | -- | unknown transaction name
    ChainBadTxName String
  | -- | wrong arg count / type mismatch (caller side)
    ChainBadTxArgs String
  | -- | type-checker rejected the contract
    ChainTypeMismatch String
  deriving stock (Eq, Show)

renderChainError :: ChainError -> String
renderChainError = \case
  ChainParentUnknown p -> "unknown parent block id: " ++ show p
  ChainParentMissing -> "non-genesis block has no parent"
  ChainBadTxName n -> "unknown transaction: " ++ n
  ChainBadTxArgs s -> "bad transaction arguments: " ++ s
  ChainTypeMismatch s -> "contract rejected by type checker: " ++ s
