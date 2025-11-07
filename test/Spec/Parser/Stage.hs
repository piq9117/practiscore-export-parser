{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Parser.Stage where

import Practiscore.Parser.Stage (stagesWithFieldName)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (runParser)

stageParserSpec :: Spec
stageParserSpec = do
  describe "StageParser" $ do
    let header :: String = "F Number,Guntype,Minimum Rounds,Maximum Points,Classifier,Classifier_No,Stage_name,ScoringType,TimesRun"
    let stageLineData :: String = "G 1,Pistol,28,140,No,,I Am A Meat Popsicle,Comstock,1"

    it "stagesWithFieldName" $
      shouldBe
        (runParser stagesWithFieldName mempty (header <> "\n" <> stageLineData <> "\n"))
        ( Right
            [ [ ("Number", "1"),
                ("Guntype", "Pistol"),
                ("Minimum Rounds", "28"),
                ("Maximum Points", "140"),
                ("Classifier", "No"),
                ("Classifier_No", ""),
                ("Stage_name", "I Am A Meat Popsicle"),
                ("ScoringType", "Comstock"),
                ("TimesRun", "1")
              ]
            ]
        )

testTree :: IO TestTree
testTree = testSpec "Parser.Stage" $ do
  stageParserSpec
