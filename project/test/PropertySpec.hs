module PropertySpec (spec) where

import BlockChain.Ledger
import BlockChain.Parser (parseProgram)
import BlockChain.Syntax
import BlockChain.Value (Address, Value (..), mkVMap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import LedgerSpec (mintTx, tokenContract, tokenSrc, transferTx)
import Test.Hspec
import Test.QuickCheck

------------------------------------------------------------
-- Property 1: pretty . parse = id  (round-trip)
--
-- Generate small but well-formed contracts and check that re-parsing the
-- pretty-printed source yields the same AST.
------------------------------------------------------------

newtype SmallName = SmallName String
  deriving stock (Eq, Show)

instance Arbitrary SmallName where
  arbitrary = SmallName . (: []) <$> elements ['a' .. 'f'] -- single-char names to avoid reserved clash

newtype TinyTxName = TinyTxName String
  deriving stock (Eq, Show)

instance Arbitrary TinyTxName where
  arbitrary = TinyTxName <$> elements ["t1", "t2", "zz", "q", "m", "n"]

instance Arbitrary Type where
  arbitrary = elements [TInt, TBool, TAddress]

instance Arbitrary Literal where
  arbitrary =
    oneof
      [ LInt . abs <$> arbitrary,
        LBool <$> arbitrary
      ]

instance Arbitrary Op where
  arbitrary = elements [OAdd, OSub, OMul, OLt, OGt, OLe, OGe, OEq, ONe]

instance Arbitrary UOp where
  arbitrary = elements [UNeg, UNot]

-- Expressions: tightly bounded to avoid blowup. Max 2-deep AST.
instance Arbitrary Expr where
  arbitrary = sized genExpr
    where
      genExpr 0 =
        oneof
          [ pure (EVar "x"),
            ELit <$> arbitrary,
            pure ESender
          ]
      genExpr n =
        oneof
          [ genExpr 0,
            EBinOp OAdd <$> genExpr 0 <*> genExpr 0,
            EBinOp OSub <$> genExpr 0 <*> genExpr 0,
            EIndex (EVar "m") <$> genExpr 0,
            EUnary UNeg <$> genExpr 0
          ]

instance Arbitrary Stmt where
  arbitrary =
    oneof
      [ SAssign (EVar "x") <$> arbitrary,
        SRequire <$> arbitrary
      ]

instance Arbitrary StateVar where
  arbitrary = StateVar <$> (unName <$> arbitrary) <*> pure TInt <*> arbitrary
    where
      unName (SmallName s) = s

instance Arbitrary TxDef where
  arbitrary =
    TxDef
      <$> (unTxName <$> arbitrary)
      <*> pure []
      <*> scale (min 3) (listOf arbitrary)
    where
      unTxName (TinyTxName s) = s

instance Arbitrary Contract where
  arbitrary =
    Contract
      <$> elements ["C", "D", "E"]
      <*> scale (min 2) (listOf arbitrary)
      <*> scale (min 2) (listOf arbitrary)

------------------------------------------------------------
-- Property 2: total supply is conserved across transfers
--
-- For the Token contract: mint to a single address, then submit a sequence
-- of @transfer(from -> to, amount)@ txs where @from@ is always a known funded
-- address. After any such sequence, sum of balances equals supply.
--
-- We additionally check that no @transfer@ may increase the sum of balances.
------------------------------------------------------------

-- | A short sender / recipient / amount triple for one transfer.
data Transfer = Transfer Address Address Int
  deriving stock (Eq, Show)

instance Arbitrary Transfer where
  arbitrary =
    Transfer
      <$> elements ["a", "b", "c", "d"]
      <*> elements ["a", "b", "c", "d"]
      <*> chooseInt (0, 100)

-- | Build a submitted transfer tx from a generated 'Transfer'.
mkTfr :: Transfer -> SubmittedTx
mkTfr (Transfer from to amount) = transferTx from to amount

spec :: Spec
spec = do
  describe "round-trip property" $ do
    it "parseProgram . prettyContract = pure  (for generated contracts)" $
      property $ \c ->
        case parseProgram (prettyContract c) of
          Right c' -> c' === c
          Left e ->
            counterexample
              ( "round-trip failed:\n--- source ---\n"
                  ++ prettyContract c
                  ++ "\n--- error ---\n"
                  ++ show e
              )
              False

  describe "supply-conservation property" $ do
    it "for the Token contract, sum of balances == supply after mint" $
      property $ \initialAmount ->
        initialAmount >= 0 ==>
          let Right ch0 = initChain tokenContract "deployer"
              b1 = Block (Just 0) [mintTx "deployer" "a" initialAmount]
              Right (ch1, _) = submitBlock ch0 b1
           in sumBalances ch1 === initialAmount .&&. supplyOf ch1 === Just initialAmount

    it "for the Token contract, sum of balances is preserved across any transfer sequence" $
      property $ \(NonNegative (minted :: Int)) transfers ->
        minted >= 0 ==>
          let Right ch0 = initChain tokenContract "deployer"
              b1 = Block (Just 0) [mintTx "deployer" "a" minted]
              Right (ch1, _) = submitBlock ch0 b1
              txs = map mkTfr transfers
              b2 = Block (Just 1) txs
              Right (ch2, _) = submitBlock ch1 b2
           in collect (length transfers) $
                sumBalances ch2 === minted
                  .&&. supplyOf ch2 === Just minted

    it "for the Token contract, no sequence of transfers can ever create balance" $
      property $ \(NonNegative (minted :: Int)) transfers ->
        let Right ch0 = initChain tokenContract "deployer"
            b1 = Block (Just 0) [mintTx "deployer" "a" (max 0 minted)]
            Right (ch1, _) = submitBlock ch0 b1
            txs = map mkTfr transfers
            b2 = Block (Just 1) txs
            Right (ch2, _) = submitBlock ch1 b2
         in sumBalances ch2 <= max 0 minted

------------------------------------------------------------
-- Store helpers (mirror LedgerSpec; kept local for tabular cleanliness)
------------------------------------------------------------

sumBalances :: Chain -> Int
sumBalances ch =
  case Map.lookup "balances" (cStore ch) of
    Just (VMap _ inner) -> sum [n | (VInt n) <- Map.elems inner]
    _ -> 0

supplyOf :: Chain -> Maybe Int
supplyOf ch = case Map.lookup "supply" (cStore ch) of
  Just (VInt n) -> Just n
  _ -> Nothing
