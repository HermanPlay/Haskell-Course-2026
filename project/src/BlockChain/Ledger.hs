module BlockChain.Ledger
  ( BlockId,
    Block (..),
    SubmittedTx (..),
    Chain (..),
    TxLog (..),
    BlockResult (..),

    -- * Lifecycle
    initChain,
    submitBlock,
  )
where

import BlockChain.Error (ChainError (..), VMError (..))
import BlockChain.Syntax
import BlockChain.Type (checkContract)
import BlockChain.VM (TxResult (..), initStateStore, runTransaction)
import BlockChain.Value (Address, Store, Value (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

------------------------------------------------------------
-- Types
------------------------------------------------------------

type BlockId = Int

-- | A block: parent reference + a list of transactions in order.
data Block = Block
  { -- | 'Nothing' only valid for genesis
    bParent :: Maybe BlockId,
    bTxs :: [SubmittedTx]
  }
  deriving stock (Eq, Show)

-- | One transaction submitted within a block.
data SubmittedTx = SubmittedTx
  { -- | which transaction of the contract to call
    stName :: String,
    -- | who is @sender@
    stSender :: Address,
    -- | arguments by name
    stArgs :: [(String, Value)]
  }
  deriving stock (Eq, Show)

-- | The chain: blocks + the genesis + the current head + live store.
data Chain = Chain
  { cBlocks :: !(Map BlockId Block),
    cHead :: !BlockId,
    -- | next block id to mint
    cNext :: !BlockId,
    -- | current state
    cStore :: !Store,
    -- | static contract for resolving tx names
    cContract :: !Contract
  }
  deriving stock (Show)

-- | Per-tx outcome log (committed/reverted, plus why).
data TxLog = TxLog
  { tlName :: String,
    tlSender :: Address,
    tlResult :: Either VMError ()
  }
  deriving stock (Eq, Show)

-- | Result of submitting a block.
data BlockResult = BlockResult
  { -- | one entry per submitted tx, in order
    brLog :: [TxLog],
    -- | the store after all committed txs in the block
    brFinalStore :: Store,
    -- | the id assigned to the block (if accepted)
    brBlockId :: BlockId
  }
  deriving stock (Show)

------------------------------------------------------------
-- Value <-> Type matching
------------------------------------------------------------

-- | Check whether a runtime value conforms to a declared type.
-- Used to validate transaction arguments before execution.
valueMatchesType :: Type -> Value -> Bool
valueMatchesType TInt (VInt _) = True
valueMatchesType TBool (VBool _) = True
valueMatchesType TAddress (VAddr _) = True
valueMatchesType (TMap kt vt) (VMap def inner) =
  valueMatchesType vt def
    && Map.foldrWithKey
      ( \k v acc ->
          acc
            && valueMatchesType kt k
            && valueMatchesType vt v
      )
      True
      inner
valueMatchesType _ _ = False

------------------------------------------------------------
-- Initialising a chain
------------------------------------------------------------

-- | Create a new blockchain from a parsed contract. Performs type-checking
-- of the contract and initialises its state variables.
initChain :: Contract -> Address -> Either ChainError Chain
initChain contract deployer =
  case checkContract contract of
    Left te -> Left (ChainTypeMismatch (show te))
    Right () -> case initStateStore contract deployer of
      Left ve -> Left (ChainTypeMismatch (show ve))
      Right st ->
        Right $
          Chain
            { cBlocks = Map.singleton 0 genesis,
              cHead = 0,
              cNext = 1,
              cStore = st,
              cContract = contract
            }
  where
    genesis = Block {bParent = Nothing, bTxs = []}

------------------------------------------------------------
-- Submitting a block
------------------------------------------------------------

-- | Submit a block of transactions to the chain.
-- Verifies the parent reference; runs each transaction in order against the
-- current store; committed txs mutate the store, reverted txs leave it
-- unchanged and the rest of the block proceeds.
submitBlock :: Chain -> Block -> Either ChainError (Chain, BlockResult)
submitBlock chain block = do
  -- Chain invariant: parent reference must point at an existing block, and
  -- only the genesis block is allowed to have no parent.
  checkParent chain block

  -- Run all transactions, threading the store forward.
  let (logs, store') = runBlockTxs (cContract chain) (cStore chain) (bTxs block)

  let bid = cNext chain
  let chain' =
        chain
          { cBlocks = Map.insert bid block (cBlocks chain),
            cHead = bid,
            cNext = bid + 1,
            cStore = store'
          }
  let result =
        BlockResult
          { brLog = logs,
            brFinalStore = store',
            brBlockId = bid
          }
  pure (chain', result)

-- | Verify the parent reference of a block.
checkParent :: Chain -> Block -> Either ChainError ()
checkParent chain block =
  case bParent block of
    Nothing -> Left ChainParentMissing
    Just p
      | Map.member p (cBlocks chain) -> Right ()
      | otherwise -> Left (ChainParentUnknown p)

-- | Run a block's transactions in order; committed updates propagate forward,
-- reverted txs do not modify the store (rollback is implicit in 'runTransaction').
runBlockTxs :: Contract -> Store -> [SubmittedTx] -> ([TxLog], Store)
runBlockTxs contract = go
  where
    go st0 [] = ([], st0)
    go st0 (tx : rest) =
      let (logEntry, mbNewStore) = runOne st0 tx
       in case mbNewStore of
            Just st1 ->
              let (logs, st2) = go st1 rest
               in (logEntry : logs, st2)
            Nothing ->
              let (logs, st2) = go st0 rest
               in (logEntry : logs, st2)

    runOne :: Store -> SubmittedTx -> (TxLog, Maybe Store)
    runOne st (SubmittedTx name sender args) =
      case findTxDef name contract of
        Nothing ->
          (TxLog name sender (Left (VMRevert "unknown tx")), Nothing)
        Just txDef ->
          case checkArgs txDef args of
            Left why ->
              (TxLog name sender (Left (VMRevert why)), Nothing)
            Right () ->
              case runTransaction txDef args sender st of
                TxCommitted st' ->
                  (TxLog name sender (Right ()), Just st')
                TxReverted e ->
                  (TxLog name sender (Left e), Nothing)

-- | Look up a transaction definition by name in the contract.
findTxDef :: String -> Contract -> Maybe TxDef
findTxDef name = foldr step Nothing . cTxs
  where
    step txDef acc
      | txName txDef == name = Just txDef
      | otherwise = acc

-- | Validate that accepted arguments conform to the transaction's signature.
-- Missing/extra args or type mismatches produce a string error.
checkArgs :: TxDef -> [(String, Value)] -> Either String ()
checkArgs txDef args
  | length params /= length args =
      Left
        ( "wrong number of arguments: got "
            ++ show (length args)
            ++ ", expected "
            ++ show (length params)
        )
  | otherwise = case [ (p, v) | (p, v) <- zip params args, pName p /= fst v || not (valueMatchesType (pType p) (snd v))
                     ] of
      [] -> Right ()
      ((p, v) : _) ->
        Left
          ( "argument mismatch for "
              ++ pName p
              ++ ": expected type "
              ++ show (pType p)
              ++ ", got value "
              ++ show (snd v)
          )
  where
    params = txParams txDef
    pName (n, _) = n
    pType (_, t) = t
