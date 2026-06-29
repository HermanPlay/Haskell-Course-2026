module VMSpec (spec) where

import BlockChain.Error (VMError (..))
import BlockChain.Parser (parseProgram)
import BlockChain.Syntax
import BlockChain.VM
  ( Eval,
    EvalState (..),
    TxResult (..),
    evalExpr,
    execStmt,
    initStateStore,
    runEvalPure,
    runTransaction,
  )
import BlockChain.Value (Address, TxEnv (..), Value (..), mkVMap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import ParserSpec (simpleCoinSrc)
import Test.Hspec

type Store = Map String Value

-- | Build a small no-parameter transaction fixture.
mkTx :: String -> [Stmt] -> TxDef
mkTx name body = TxDef name [] body

-- | A singleton int state var.
store :: String -> Int -> Store
store n i = Map.singleton n (VInt i)

-- | A singleton map<addr,int> state var prefilled (default 0 for missing keys).
storeMap :: String -> [(Address, Int)] -> Store
storeMap nm kvs =
  Map.singleton nm (mkVMap (VInt 0) [(VAddr a, VInt i) | (a, i) <- kvs])

-- | Run an 'Eval' with explicit (sender, params) env + store, returning
-- the result and the resulting store.
runWith ::
  Maybe Address ->
  [(String, Value)] ->
  Store ->
  Eval a ->
  Either VMError (a, Store)
runWith mbSender params st0 m =
  case runEvalPure m (EvalState st0 env) of
    Left e -> Left e
    Right (a, st') -> Right (a, esStore st')
  where
    env = TxEnv {txeSender = mbSender, txeParams = Map.fromList params}

spec :: Spec
spec = do
  describe "evalExpr" $ do
    it "evaluates int literals" $
      runWith Nothing [] Map.empty (evalExpr (ELit (LInt 42)))
        `shouldBe` Right (VInt 42, Map.empty :: Store)

    it "evaluates bool literals" $
      runWith Nothing [] Map.empty (evalExpr (ELit (LBool True)))
        `shouldBe` Right (VBool True, Map.empty :: Store)

    it "evaluates ESender when an env has one" $
      runWith (Just "alice") [] Map.empty (evalExpr ESender)
        `shouldBe` Right (VAddr "alice", Map.empty :: Store)

    it "errors on ESender when none is set" $
      runWith Nothing [] Map.empty (evalExpr ESender)
        `shouldSatisfy` \r -> case r of Left VMSenderMissing -> True; _ -> False

    it "evaluates Var from state" $
      runWith Nothing [] (store "x" 7) (evalExpr (EVar "x"))
        `shouldBe` Right (VInt 7, store "x" 7)

    it "evaluates Var from params (overrides state)" $
      let me = store "x" 7
       in runWith Nothing [("x", VInt 999)] me (evalExpr (EVar "x"))
            `shouldBe` Right (VInt 999, me)

    it "errors on unbound Var" $
      runWith Nothing [] Map.empty (evalExpr (EVar "x"))
        `shouldSatisfy` \r -> case r of Left (VMUnboundVar "x") -> True; _ -> False

    it "evaluates binary arithmetic" $ do
      let e = EBinOp OAdd (ELit (LInt 2)) (ELit (LInt 3))
      runWith Nothing [] Map.empty (evalExpr e)
        `shouldBe` Right (VInt 5, Map.empty :: Store)

    it "throws on division by zero" $ do
      let e = EBinOp ODiv (ELit (LInt 7)) (ELit (LInt 0))
      runWith Nothing [] Map.empty (evalExpr e)
        `shouldSatisfy` \r -> case r of Left VMDivByZero -> True; _ -> False

    it "errors when adding non-int values" $ do
      let e = EBinOp OAdd (ELit (LBool True)) (ELit (LInt 1))
      runWith Nothing [] Map.empty (evalExpr e)
        `shouldSatisfy` \r -> case r of Left (VMTypeError _) -> True; _ -> False

    it "evaluates comparisons" $ do
      let e1 = EBinOp OLe (ELit (LInt 3)) (ELit (LInt 3))
      runWith Nothing [] Map.empty (evalExpr e1)
        `shouldBe` Right (VBool True, Map.empty :: Store)
      let e2 = EBinOp OLt (ELit (LInt 3)) (ELit (LInt 3))
      runWith Nothing [] Map.empty (evalExpr e2)
        `shouldBe` Right (VBool False, Map.empty :: Store)

    it "evaluates equality on ints" $ do
      let e = EBinOp OEq (EVar "a") (EVar "b")
      runWith Nothing [("a", VInt 7), ("b", VInt 7)] Map.empty (evalExpr e)
        `shouldBe` Right (VBool True, Map.empty :: Store)

    it "evaluates a missing map key to the default value" $ do
      let st = storeMap "bal" [] -- empty map with default 0
          e = EIndex (EVar "bal") (EVar "who")
      runWith Nothing [("who", VAddr "anyone")] st (evalExpr e)
        `shouldBe` Right (VInt 0, st)

    it "evaluates an existing map key" $ do
      let st = storeMap "bal" [("bob", 50)]
          e = EIndex (EVar "bal") (EVar "k")
      runWith Nothing [("k", VAddr "bob")] st (evalExpr e)
        `shouldBe` Right (VInt 50, st)

  describe "execStmt" $ do
    it "assigns to a state var" $
      runWith
        Nothing
        []
        (store "x" 0)
        (execStmt (SAssign (EVar "x") (ELit (LInt 5))))
        `shouldBe` Right ((), store "x" 5)

    it "assigns to map[k]" $ do
      let st = storeMap "bal" []
          stmt = SAssign (EIndex (EVar "bal") (ESender)) (ELit (LInt 100))
          expected = Map.singleton "bal" (mkVMap (VInt 0) [(VAddr "alice", VInt 100)])
      runWith (Just "alice") [] st (execStmt stmt) `shouldBe` Right ((), expected)

    it "executes require that passes (state unchanged)" $ do
      let st = store "x" 3
      runWith Nothing [] st (execStmt (SRequire (EBinOp OGe (EVar "x") (ELit (LInt 3)))))
        `shouldBe` Right ((), st)

    it "reverts on require failure (VMRevert)" $ do
      let st = store "x" 0
      runWith Nothing [] st (execStmt (SRequire (EBinOp OGe (EVar "x") (ELit (LInt 1)))))
        `shouldSatisfy` \r -> case r of Left (VMRevert _) -> True; _ -> False

    it "executes if-then when condition is true" $ do
      let st = store "x" 0
          stmt =
            SIf
              (EBinOp OEq (EVar "x") (ELit (LInt 0)))
              [SAssign (EVar "x") (ELit (LInt 42))]
              [SAssign (EVar "x") (ELit (LInt 7))]
      runWith Nothing [] st (execStmt stmt) `shouldBe` Right ((), store "x" 42)

    it "executes if-else when condition is false" $ do
      let st = store "x" 0
          stmt =
            SIf
              (EBinOp OEq (EVar "x") (ELit (LInt 99)))
              [SAssign (EVar "x") (ELit (LInt 42))]
              [SAssign (EVar "x") (ELit (LInt 7))]
      runWith Nothing [] st (execStmt stmt) `shouldBe` Right ((), store "x" 7)

  describe "runTransaction (rollback)" $ do
    it "commits when the transaction succeeds" $ do
      let tx = mkTx "inc" [SAssign (EVar "x") (EBinOp OAdd (EVar "x") (ELit (LInt 1)))]
      runTransaction tx [] "a" (store "x" 10) `shouldBe` TxCommitted (store "x" 11)

    it "reverts when require fails (preserves original store)" $ do
      let tx =
            mkTx
              "guarded"
              [ SAssign (EVar "x") (EBinOp OAdd (EVar "x") (ELit (LInt 100))),
                SRequire (EBinOp OGe (EVar "x") (ELit (LInt 1000)))
              ]
          original = store "x" 5
      runTransaction tx [] "a" original
        `shouldSatisfy` \r -> case r of
          TxReverted _ -> True
          TxCommitted st -> st /= original -- must NOT commit mutated state
          _ -> False

    it "rolling back means a later require failure undoes earlier mutations" $ do
      let tx =
            mkTx
              "two"
              [ SAssign (EVar "x") (ELit (LInt 999)), -- mutation A
                SRequire (EBinOp OGe (EVar "x") (ELit (LInt 100000))) -- fails
              ]
          original = store "x" 1
      case runTransaction tx [] "a" original of
        TxReverted _ -> original `shouldBe` store "x" 1 -- unchanged pattern check
        other -> expectationFailure $ "expected TxReverted, got " ++ show other

  describe "initStateStore" $ do
    it "initialises state vars of the SimpleCoin contract" $ do
      let Right c = parseProgram simpleCoinSrc
      let Right st = initStateStore c "dave"
      Map.lookup "owner" st `shouldBe` Just (VAddr "dave")
      case Map.lookup "balances" st of
        Just (VMap _ inner) -> inner `shouldBe` Map.empty
        other -> expectationFailure $ "expected VMap, got " ++ show other
