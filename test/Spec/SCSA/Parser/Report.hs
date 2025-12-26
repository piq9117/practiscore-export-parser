module Spec.SCSA.Parser.Report (testTree) where

import Practiscore.SCSA.Parser.Report (ReportFields (..), reportFields)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (runParser)

reportFieldsSpec :: Spec
reportFieldsSpec =
  describe "reportFields" $ do
    it "reportFields - infometadata" $ do
      let infoLine = "ER,pricetown,1,\"Pricetown Steel Challenge - December 2025 Sunday Match\",20251221,20251226,,,,,,,,,,,,,,,,,,,,"
      shouldBe
        (runParser reportFields mempty infoLine)
        (Right (InfoMetadata "pricetown,1,\"Pricetown Steel Challenge - December 2025 Sunday Match\",20251221,20251226,,,,,,,,,,,,,,,,,,,,"))

testTree :: IO TestTree
testTree =
  testSpec "Spec.SCSA.Parser.Report" $
    reportFieldsSpec
