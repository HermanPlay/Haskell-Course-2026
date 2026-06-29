module ParserSpec (spec, simpleCoinSrc) where

import BlockChain.Error (renderParseError)
import BlockChain.Parser (parseProgram)
import BlockChain.Syntax
import Test.Hspec

-- | A tiny contract used as a fixture throughout.
simpleCoinSrc :: String
simpleCoinSrc =
  unlines
    [ "contract SimpleCoin {",
      "  state {",
      "    balances: map<address, int> = empty;",
      "    owner:    address          = sender;",
      "  }",
      "",
      "  transaction transfer(to: address, amount: int) {",
      "    require balances[sender] >= amount;",
      "    balances[sender] := balances[sender] - amount;",
      "    balances[to]     := balances[to] + amount;",
      "  }",
      "}"
    ]

spec :: Spec
spec = do
  describe "parseProgram" $ do
    it "parses the SimpleCoin contract" $ do
      parseProgram simpleCoinSrc
        `shouldSatisfy` either
          (const False)
          ( \c ->
              cName c == "SimpleCoin"
                && length (cState c) == 2
                && length (cTxs c) == 1
          )

    it "parses state declarations" $ do
      let Right c = parseProgram simpleCoinSrc
      let [b, o] = cState c
      svName b `shouldBe` "balances"
      svType b `shouldBe` TMap TAddress TInt
      svInit b `shouldBe` ELit LEmpty
      svName o `shouldBe` "owner"
      svType o `shouldBe` TAddress
      svInit o `shouldBe` ESender

    it "parses a transaction definition with params" $ do
      let Right c = parseProgram simpleCoinSrc
      let [tx] = cTxs c
      txName tx `shouldBe` "transfer"
      txParams tx `shouldBe` [("to", TAddress), ("amount", TInt)]
      length (txBody tx) `shouldBe` 3

    it "skips line comments" $ do
      parseProgram "// hello world\ncontract C { state {} }\n"
        `shouldSatisfy` either (const False) ((== "C") . cName)

    it "skips block comments" $ do
      parseProgram "/* a\n   b\n*/contract C { state {} }"
        `shouldSatisfy` either (const False) ((== "C") . cName)

    it "reports a useful message for unclosed braces (no crash)" $ do
      let Left e = parseProgram "contract C { state {"
      renderParseError e `shouldSatisfy` (not . null)

    it "rejects a reserved word as a contract name" $ do
      parseProgram "contract require { state {} }"
        `shouldSatisfy` either (const True) (const False)

  describe "parseExpr (precedence)" $ do
    let go s = case parseProgram ("contract C { state { x: int = " ++ s ++ "; } }") of
          Right c -> case cState c of sv : _ -> Just (svInit sv)
          Left _ -> Nothing

    it "parses additive left-associatively" $ do
      go "1 + 2 - 3" `shouldBe` Just (EBinOp OSub (EBinOp OAdd (ELit (LInt 1)) (ELit (LInt 2))) (ELit (LInt 3)))

    it "binds * tighter than +" $ do
      go "2 * 3 + 4" `shouldBe` Just (EBinOp OAdd (EBinOp OMul (ELit (LInt 2)) (ELit (LInt 3))) (ELit (LInt 4)))

    it "binds - tighter than <" $ do
      go "a - b < c"
        `shouldBe` Just
          ( EBinOp
              OLt
              (EBinOp OSub (EVar "a") (EVar "b"))
              (EVar "c")
          )

    it "binds == / != below comparison" $ do
      go "a == b" `shouldBe` Just (EBinOp OEq (EVar "a") (EVar "b"))
      go "a != b" `shouldBe` Just (EBinOp ONe (EVar "a") (EVar "b"))

    it "binds && / || as expected" $ do
      go "a && b || c"
        `shouldBe` Just
          ( EBinOp
              OOr
              (EBinOp OAnd (EVar "a") (EVar "b"))
              (EVar "c")
          )

    it "parses unary minus" $ do
      go "-x" `shouldBe` Just (EUnary UNeg (EVar "x"))
      go "- -1" `shouldBe` Just (EUnary UNeg (EUnary UNeg (ELit (LInt 1))))

    it "parses map indexing" $ do
      go "balances[sender]"
        `shouldBe` Just (EIndex (EVar "balances") ESender)
      go "m[a][b]"
        `shouldBe` Just (EIndex (EIndex (EVar "m") (EVar "a")) (EVar "b"))

    it "parses parentheses as grouping" $ do
      go "(1 + 2) * 3"
        `shouldBe` Just
          ( EBinOp
              OMul
              (EBinOp OAdd (ELit (LInt 1)) (ELit (LInt 2)))
              (ELit (LInt 3))
          )

    it "parses the sender keyword as ESender" $ do
      go "sender" `shouldBe` Just ESender

    it "parses true / false / empty literals" $ do
      go "true" `shouldBe` Just (ELit (LBool True))
      go "false" `shouldBe` Just (ELit (LBool False))
      go "empty" `shouldBe` Just (ELit LEmpty)
