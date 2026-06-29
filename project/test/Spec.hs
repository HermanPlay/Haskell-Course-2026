module Main (main) where

import LedgerSpec qualified
import ParserSpec qualified
import PropertySpec qualified
import Test.Hspec
import VMSpec qualified

main :: IO ()
main = hspec $ do
  ParserSpec.spec
  VMSpec.spec
  LedgerSpec.spec
  PropertySpec.spec
