{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Parser.Stage where

import Practiscore.Parser.Stage (stageHeaderLine, stageLine)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (runParser)

stageParserSpec :: Spec
stageParserSpec = do
  describe "StageParser" $ do
    let header :: String = "F Number,Guntype,Minimum Rounds,Maximum Points,Classifier,Classifier_No,Stage_name,ScoringType,TimesRun"
    let stages =
          [ "G 1,Pistol,28,140,No,,I Am A Meat Popsicle,Comstock,1",
            "G 2,Pistol,24,120,Yes,03-18,High Standards,Virginia,2",
            "G 3,Pistol,8,40,No,,Natty Boh,Comstock,1",
            "G 4,Pistol,9,45,No,,Island Hoppers,Comstock,1",
            "G 5,Pistol,18,90,No,,Amish You Were,Comstock,1",
            "G 6,Pistol,32,160,No,,Lowered Providence,Comstock,1"
          ]

    it "stageHeaderLine" $ do
      shouldBe
        (runParser stageHeaderLine mempty header)
        ( Right
            [ "Number",
              "Guntype",
              "Minimum Rounds",
              "Maximum Points",
              "Classifier",
              "Classifier_No",
              "Stage_name",
              "ScoringType",
              "TimesRun"
            ]
        )

    it "stageLine" $
      shouldBe
        (runParser stageLine mempty <$> stages)
        [ Right ["1", "Pistol", "28", "140", "No", "", "I Am A Meat Popsicle", "Comstock", "1"],
          Right ["2", "Pistol", "24", "120", "Yes", "03-18", "High Standards", "Virginia", "2"],
          Right ["3", "Pistol", "8", "40", "No", "", "Natty Boh", "Comstock", "1"],
          Right ["4", "Pistol", "9", "45", "No", "", "Island Hoppers", "Comstock", "1"],
          Right ["5", "Pistol", "18", "90", "No", "", "Amish You Were", "Comstock", "1"],
          Right ["6", "Pistol", "32", "160", "No", "", "Lowered Providence", "Comstock", "1"]
        ]

testTree :: IO TestTree
testTree = testSpec "Parser.Stage" $ do
  stageParserSpec
