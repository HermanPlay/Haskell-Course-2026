module LedgerSpec (spec, tokenSrc, tokenContract, transferTx, mintTx) where

import BlockChain.Error (ChainError (..), VMError (..))
import BlockChain.Ledger
import BlockChain.Parser (parseProgram)
import BlockChain.Syntax
import BlockChain.Value (Address, Value (..), mkVMap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import ParserSpec (simpleCoinSrc)
import Test.Hspec

------------------------------------------------------------
-- A Token contract (mint + transfer) used for end-to-end scenarios
------------------------------------------------------------

tokenSrc :: String
tokenSrc =
  unlines
    [ "contract Token {",
      "  state {",
      "    balances: map<address, int> = empty;",
      "    supply:   int               = 0;",
      "  }",
      "",
      "  transaction mint(to: address, amount: int) {",
      "    balances[to] := balances[to] + amount;",
      "    supply       := supply + amount;",
      "  }",
      "",
      "  transaction transfer(to: address, amount: int) {",
      "    require balances[sender] >= amount;",
      "    balances[sender] := balances[sender] - amount;",
      "    balances[to]     := balances[to] + amount;",
      "  }",
      "}"
    ]

-- | A @transfer(to, amount)@ submitted tx.
transferTx :: Address -> Address -> Int -> SubmittedTx
transferTx sender to amount =
  SubmittedTx "transfer" sender [("to", VAddr to), ("amount", VInt amount)]

-- | A @mint(to, amount)@ submitted tx.
mintTx :: Address -> Address -> Int -> SubmittedTx
mintTx sender to amount =
  SubmittedTx "mint" sender [("to", VAddr to), ("amount", VInt amount)]

-- | The parsed Token contract (assumed well-formed).
tokenContract :: Contract
tokenContract = case parseProgram tokenSrc of
  Right c -> c
  Left e -> error ("token contract failed to parse: " ++ show e)

------------------------------------------------------------
-- Store helpers
------------------------------------------------------------

supplyOf :: Chain -> Maybe Int
supplyOf ch = case Map.lookup "supply" (cStore ch) of
  Just (VInt n) -> Just n
  _ -> Nothing

sumBalances :: Chain -> Int
sumBalances ch =
  case Map.lookup "balances" (cStore ch) of
    Just (VMap _ inner) -> sum [n | (VInt n) <- Map.elems inner]
    _ -> 0

balanceOf :: Chain -> Address -> Int
balanceOf ch addr =
  case Map.lookup "balances" (cStore ch) of
    Just (VMap _ inner) -> case Map.lookup (VAddr addr) inner of
      Just (VInt n) -> n
      _ -> 0
    _ -> 0

isReverted :: TxLog -> Bool
isReverted logEntry = case tlResult logEntry of
  Left _ -> True
  Right () -> False

isCommitted :: TxLog -> Bool
isCommitted = not . isReverted

------------------------------------------------------------
-- Spec
------------------------------------------------------------

spec :: Spec
spec = do
  describe "initChain" $ do
    it "type-checks and produces a genesis block with head 0" $ do
      case initChain tokenContract "deployer" of
        Left e -> expectationFailure $ "init failed: " ++ show e
        Right ch -> do
          cHead ch `shouldBe` 0
          cNext ch `shouldBe` 1
          Map.member 0 (cBlocks ch) `shouldBe` True
          case Map.lookup 0 (cBlocks ch) of
            Just b -> bParent b `shouldBe` Nothing
            Nothing -> expectationFailure "genesis block missing"

    it "rejects a contract with a type error" $ do
      let Right c = parseProgram "contract Bad { state { x: int = true; } }"
      initChain c "d" `shouldSatisfy` \r ->
        case r of Left (ChainTypeMismatch _) -> True; _ -> False

  describe "submitBlock (chain invariant)" $ do
    it "accepts a block whose parent is the current head" $ do
      case initChain tokenContract "d" of
        Right ch -> case submitBlock ch (Block (Just 0) []) of
          Right (ch', _) -> cHead ch' `shouldBe` 1
          Left e -> expectationFailure $ "rejected: " ++ show e
        Left e -> expectationFailure $ "init failed: " ++ show e

    it "rejects a block whose parent doesn't exist" $ do
      case initChain tokenContract "d" of
        Right ch ->
          submitBlock ch (Block (Just 99) [])
            `shouldSatisfy` \r -> case r of
              Left (ChainParentUnknown 99) -> True
              _ -> False
        Left e -> expectationFailure $ "init failed: " ++ show e

    it "rejects a non-genesis block with no parent" $ do
      case initChain tokenContract "d" of
        Right ch ->
          submitBlock ch (Block Nothing [])
            `shouldSatisfy` \r -> case r of
              Left ChainParentMissing -> True
              _ -> False
        Left e -> expectationFailure $ "init failed: " ++ show e

    it "does not advance head when the parent is wrong" $ do
      case initChain tokenContract "d" of
        Right ch -> case submitBlock ch (Block (Just 5) []) of
          Left _ -> cHead ch `shouldBe` 0
          Right _ -> expectationFailure "expected rejection"
        Left e -> expectationFailure $ "init failed: " ++ show e

  describe "submitBlock (unknown / bad signature txs)" $ do
    it "logs a reverted tx for an unknown transaction name" $ do
      case initChain tokenContract "d" of
        Right ch -> case submitBlock ch (Block (Just 0) [SubmittedTx "nope" "d" []]) of
          Right (_, r) -> case brLog r of
            [TxLog {tlName = "nope", tlResult = Left _}] -> return ()
            other -> expectationFailure $ "unexpected log: " ++ show other
          Left e -> expectationFailure $ "rejected: " ++ show e
        Left e -> expectationFailure $ "init failed: " ++ show e

    it "logs a reverted tx for an arg-count mismatch" $ do
      case initChain tokenContract "d" of
        Right ch -> case submitBlock
          ch
          ( Block
              (Just 0)
              [SubmittedTx "mint" "d" [("to", VAddr "x")]]
          ) of
          Right (_, r) -> case brLog r of
            [TxLog {tlName = "mint", tlResult = Left _}] -> return ()
            other -> expectationFailure $ "unexpected log: " ++ show other
          Left e -> expectationFailure $ "rejected: " ++ show e
        Left e -> expectationFailure $ "init failed: " ++ show e

    it "rejects a type-mismatched argument at call-time" $ do
      case initChain tokenContract "deployer" of
        Right ch -> case submitBlock
          ch
          ( Block
              (Just 0)
              [ SubmittedTx
                  "mint"
                  "deployer"
                  [("to", VInt 9), ("amount", VInt 1)]
              ]
          ) of
          Right (_, r) -> case brLog r of
            [TxLog {tlName = "mint", tlResult = Left _}] -> return ()
            other -> expectationFailure $ "unexpected log: " ++ show other
          Left e -> expectationFailure $ "rejected: " ++ show e
        Left e -> expectationFailure $ "init failed: " ++ show e

  describe "submitBlock (end-to-end scenarios)" $ do
    it "scenario 1: a single mint produces the expected supply" $ do
      case initChain tokenContract "deployer" of
        Right ch -> case submitBlock ch (Block (Just 0) [mintTx "deployer" "alice" 100]) of
          Right (ch1, _) -> do
            balanceOf ch1 "alice" `shouldBe` 100
            supplyOf ch1 `shouldBe` Just 100
            sumBalances ch1 `shouldBe` 100
          Left e -> expectationFailure $ "rejected: " ++ show e
        Left e -> expectationFailure $ "init failed: " ++ show e

    it "scenario 2: chain of transfers preserves supply" $ do
      case initChain tokenContract "deployer" of
        Right ch0 -> do
          ch1 <- mintChain ch0 (mintTx "deployer" "alice" 1000)
          -- Block 2: alice -> bob 300, alice -> carol 500, bob -> carol 50
          ch2 <-
            submitChain
              ch1
              ( Block
                  (Just 1)
                  [ transferTx "alice" "bob" 300,
                    transferTx "alice" "carol" 500,
                    transferTx "bob" "carol" 50
                  ]
              )
          balanceOf ch2 "alice" `shouldBe` 200 -- 1000 - 300 - 500
          balanceOf ch2 "bob" `shouldBe` 250 -- 300 - 50
          balanceOf ch2 "carol" `shouldBe` 550 -- 500 + 50
          sumBalances ch2 `shouldBe` 1000
          supplyOf ch2 `shouldBe` Just 1000
        Left e -> expectationFailure $ "init failed: " ++ show e

    it "scenario 3: double-spend attempt is rejected; state unaffected" $ do
      case initChain tokenContract "deployer" of
        Right ch0 -> do
          ch1 <- mintChain ch0 (mintTx "deployer" "alice" 300)
          -- Block 2: alice sends 200 to bob AND 200 to carol — only one can win.
          (ch2, r2) <-
            submitChainLog
              ch1
              ( Block
                  (Just 1)
                  [ transferTx "alice" "bob" 200,
                    transferTx "alice" "carol" 200
                  ]
              )
          balanceOf ch2 "alice" `shouldBe` 100 -- 300 - 200 (first wins)
          balanceOf ch2 "bob" `shouldBe` 200
          balanceOf ch2 "carol" `shouldBe` 0
          length (filter isReverted (brLog r2)) `shouldBe` 1
          length (filter isCommitted (brLog r2)) `shouldBe` 1
        Left e -> expectationFailure $ "init failed: " ++ show e

    it "scenario 4: mid-block revert doesn't corrupt subsequent txs" $ do
      case initChain tokenContract "deployer" of
        Right ch0 -> do
          ch1 <- mintChain ch0 (mintTx "deployer" "alice" 100)
          ch2 <-
            submitChain
              ch1
              ( Block
                  (Just 1)
                  [ transferTx "alice" "bob" 40, -- ok
                    transferTx "alice" "carol" 999, -- revert (alice has 60)
                    transferTx "alice" "bob" 20 -- ok (alice has 60)
                  ]
              )
          balanceOf ch2 "alice" `shouldBe` 40 -- 100 - 40 - 20
          balanceOf ch2 "bob" `shouldBe` 60
          balanceOf ch2 "carol" `shouldBe` 0
          sumBalances ch2 `shouldBe` 100
        Left e -> expectationFailure $ "init failed: " ++ show e

    it "scenario 5: bad parent reference leaves chain state unchanged" $ do
      case initChain tokenContract "deployer" of
        Right ch0 -> do
          ch1 <- mintChain ch0 (mintTx "deployer" "a" 5)
          -- Submit a block with an unknown parent.
          case submitBlock ch1 (Block (Just 42) [mintTx "deployer" "b" 100]) of
            Left (ChainParentUnknown 42) -> do
              cHead ch1 `shouldBe` 1
              balanceOf ch1 "a" `shouldBe` 5
              balanceOf ch1 "b" `shouldBe` 0
            other -> expectationFailure $ "expected ChainParentUnknown, got " ++ show other
        Left e -> expectationFailure $ "init failed: " ++ show e

    it "scenario 6: a multi-block saga preserves supply across blocks" $ do
      case initChain tokenContract "deployer" of
        Right ch0 -> do
          ch1 <- mintChain ch0 (mintTx "deployer" "alice" 500)
          ch2 <-
            submitChain
              ch1
              ( Block
                  (Just 1)
                  [ transferTx "alice" "bob" 100,
                    transferTx "alice" "carol" 50
                  ]
              )
          ch3 <-
            submitChain
              ch2
              ( Block
                  (Just 2)
                  [ transferTx "bob" "carol" 25,
                    transferTx "carol" "alice" 30,
                    transferTx "alice" "bob" 999 -- revert (alice has 320)
                  ]
              )
          balanceOf ch3 "alice" `shouldBe` 380 -- 500-100-50+30
          balanceOf ch3 "bob" `shouldBe` 75 -- 100-25
          balanceOf ch3 "carol" `shouldBe` 45 -- 50+25-30
          sumBalances ch3 `shouldBe` 500
        Left e -> expectationFailure $ "init failed: " ++ show e

------------------------------------------------------------
-- Spec helpers: monadic submit
------------------------------------------------------------

-- Shorthand: submit a single-tx block to mint initial supply.
mintChain :: Chain -> SubmittedTx -> IO Chain
mintChain ch tx = submitChain ch (Block (Just (cHead ch)) [tx])

-- Shorthand: submit a block; fail the test if rejected.
submitChain :: Chain -> Block -> IO Chain
submitChain ch b = case submitBlock ch b of
  Right (ch', _) -> pure ch'
  Left e -> do
    expectationFailure ("block rejected: " ++ show e)
    error "unreachable"

-- Shorthand: submit a block, returning both the new chain and the block result.
submitChainLog :: Chain -> Block -> IO (Chain, BlockResult)
submitChainLog ch b = case submitBlock ch b of
  Right r -> pure r
  Left e -> do
    expectationFailure ("block rejected: " ++ show e)
    error "unreachable"
