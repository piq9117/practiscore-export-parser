module Spec.Core.Parser (testTree) where

import Practiscore.Parser (cells)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (eof, runParser)

coreParserSpec :: Spec
coreParserSpec =
  describe "core parser" $ do
    it "cells - parse individual cells" $ do
      let line = ",hello,world,\"this,should,be,one,cell\""

      shouldBe
        (runParser (cells <* eof) mempty line)
        (Right ["", "hello", "world", "this,should,be,one,cell"])

testTree :: IO TestTree
testTree = testSpec "Spec.Core.Parser" $ do
  coreParserSpec
