module Spec.Parser.Report (testTree) where

import Practiscore.Parser.Report (info, matchSummary, title, zMetadata)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (runParser)

reportParserSpec :: Spec
reportParserSpec =
  describe "ReportParser" $ do
    it "title" $ do
      shouldBe
        (runParser title mempty "$PRACTISCORE 1.682 RESULTS\n")
        (Right "1.682 RESULTS")
    it "info" $ do
      let infoLines = "$INFO Region:USPSA\n$INFO PLATFORM IOS\n$INFO PractiScore_Product: PractiScore (iOS)\n$INFO PractiScore_Version:1.682\n$INFO Match name:LPRGC - Lower Providence USPSA Match November 2025  Match\n$INFO Match date:11/01/2025\n$INFO Club Name:LPRGC\n$INFO Club Code:MID04\n$INFO Multigun:0\n$INFO Scoring:0\n$INFO Classifiers:1\n$INFO Match Level: Level I\n"
      shouldBe
        (runParser info mempty infoLines)
        ( Right
            [ "Region:USPSA",
              "PLATFORM IOS",
              "PractiScore_Product: PractiScore (iOS)",
              "PractiScore_Version:1.682",
              "Match name:LPRGC - Lower Providence USPSA Match November 2025  Match",
              "Match date:11/01/2025",
              "Club Name:LPRGC",
              "Club Code:MID04",
              "Multigun:0",
              "Scoring:0",
              "Classifiers:1",
              "Match Level: Level I"
            ]
        )

    it "zMetadata" $ do
      shouldBe
        (runParser zMetadata mempty "Z 3.0\n")
        (Right "3.0")

    it "matchSummary" $
      shouldBe
        (runParser matchSummary mempty "A LPRGC - Lower Providence USPSA Match November 2025  Match,LPRGC,11/01/2025\n")
        (Right "LPRGC - Lower Providence USPSA Match November 2025  Match,LPRGC,11/01/2025")

testTree :: IO TestTree
testTree = testSpec "Parser.Report" $ do
  reportParserSpec
