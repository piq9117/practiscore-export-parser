module Spec.USPSA.Parser.Score (testTree) where

import Practiscore.USPSA.Parser.Score
  ( scoreHeader,
    scoreLine,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (runParser)

scoreParserSpec :: Spec
scoreParserSpec =
  describe "ScoreParser" $ do
    let header = "H Gun,Stage,Comp,DQ,DNF,A,B,C,D,Miss,No Shoot,Procedural,Double Poppers,Double Popper Miss,Late Shot,Extra Shot,Extra Hit,No Penalty Miss,Additional Penalty,Total Penalty,T1,T2,T3,T4,T5,Time,Raw Points,Total Points,Hit Factor,Stage Points,Stage Place,Stage Power Factor"

    it "scoreHeader" $ do
      shouldBe
        (runParser scoreHeader mempty header)
        ( Right
            [ "Gun",
              "Stage",
              "Comp",
              "DQ",
              "DNF",
              "A",
              "B",
              "C",
              "D",
              "Miss",
              "No Shoot",
              "Procedural",
              "Double Poppers",
              "Double Popper Miss",
              "Late Shot",
              "Extra Shot",
              "Extra Hit",
              "No Penalty Miss",
              "Additional Penalty",
              "Total Penalty",
              "T1",
              "T2",
              "T3",
              "T4",
              "T5",
              "Time",
              "Raw Points",
              "Total Points",
              "Hit Factor",
              "Stage Points",
              "Stage Place",
              "Stage Power Factor"
            ]
        )

    it "scoreLine" $ do
      let scoreLines =
            [ "I Pistol,1,1,No,No,26,0,2,0,0,0,0,20,0,0,0,0,0,0,0,20.54,0,0,0,0,20.54,136,136,6.6212,140.0000,1,",
              "I Pistol,1,2,No,No,26,0,1,0,1,0,0,20,0,0,0,0,0,0,10,24.34,0,0,0,0,24.34,133,123,5.0534,106.8501,2,",
              "I Pistol,1,3,No,No,27,0,0,0,1,0,0,20,0,0,0,0,0,0,10,29.25,0,0,0,0,29.25,135,125,4.2735,90.3598,5,"
            ]
      shouldBe
        (runParser scoreLine mempty <$> scoreLines)
        [ Right ["Pistol", "1", "1", "No", "No", "26", "0", "2", "0", "0", "0", "0", "20", "0", "0", "0", "0", "0", "0", "0", "20.54", "0", "0", "0", "0", "20.54", "136", "136", "6.6212", "140.0000", "1", ""],
          Right ["Pistol", "1", "2", "No", "No", "26", "0", "1", "0", "1", "0", "0", "20", "0", "0", "0", "0", "0", "0", "10", "24.34", "0", "0", "0", "0", "24.34", "133", "123", "5.0534", "106.8501", "2", ""],
          Right ["Pistol", "1", "3", "No", "No", "27", "0", "0", "0", "1", "0", "0", "20", "0", "0", "0", "0", "0", "0", "10", "29.25", "0", "0", "0", "0", "29.25", "135", "125", "4.2735", "90.3598", "5", ""]
        ]

testTree :: IO TestTree
testTree = testSpec "Parser.Score" $ do
  scoreParserSpec
