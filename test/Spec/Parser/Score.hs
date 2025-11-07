module Spec.Parser.Score (testTree) where

import Practiscore.Parser.Score
  ( Score (..),
    decodeScores,
    scoreHeader,
    scoresWithFieldName,
  )
import Practiscore.USPSA (CompId (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (runParser)

scoreParserSpec :: Spec
scoreParserSpec =
  describe "ScoreParser" $ do
    let header = "H Gun,Stage,Comp,DQ,DNF,A,B,C,D,Miss,No Shoot,Procedural,Double Poppers,Double Popper Miss,Late Shot,Extra Shot,Extra Hit,No Penalty Miss,Additional Penalty,Total Penalty,T1,T2,T3,T4,T5,Time,Raw Points,Total Points,Hit Factor,Stage Points,Stage Place,Stage Power Factor"
    let scoreLines = "I Pistol,1,1,No,No,26,0,2,0,0,0,0,20,0,0,0,0,0,0,0,20.54,0,0,0,0,20.54,136,136,6.6212,140.0000,1,\nI Pistol,1,2,No,No,26,0,1,0,1,0,0,20,0,0,0,0,0,0,10,24.34,0,0,0,0,24.34,133,123,5.0534,106.8501,2,"

    it "decodeScores" $ do
      shouldBe
        (runParser decodeScores mempty (header <> "\n" <> scoreLines <> "\n"))
        ( Right
            [ Score
                { gun = "Pistol",
                  stage = Just 1,
                  comp = Just CompId {unCompId = 1},
                  dQ = "No",
                  dNF = "No",
                  a = 26,
                  b = 0,
                  c = 2,
                  d = 0,
                  miss = 0,
                  noShoot = 0,
                  procedural = 0,
                  doublePoppers = 20,
                  doublePopperMiss = 0,
                  lateShot = 0,
                  extraShot = 0,
                  extraHit = 0,
                  noPenaltyMiss = 0,
                  additionalPenalty = 0,
                  totalPenalty = 0,
                  t1 = Nothing,
                  t2 = Just 0,
                  t3 = Just 0,
                  t4 = Just 0,
                  t5 = Just 0,
                  time = Just 20.54,
                  rawPoints = Just 136,
                  totalPoints = Just 136,
                  hitFactor = Just 6.6212,
                  stagePoints = Just 140.0,
                  stagePlace = Just 1,
                  stagePowerFactor = Nothing
                },
              Score
                { gun = "Pistol",
                  stage = Just 1,
                  comp = Just CompId {unCompId = 2},
                  dQ = "No",
                  dNF = "No",
                  a = 26,
                  b = 0,
                  c = 1,
                  d = 0,
                  miss = 1,
                  noShoot = 0,
                  procedural = 0,
                  doublePoppers = 20,
                  doublePopperMiss = 0,
                  lateShot = 0,
                  extraShot = 0,
                  extraHit = 0,
                  noPenaltyMiss = 0,
                  additionalPenalty = 0,
                  totalPenalty = 10,
                  t1 = Nothing,
                  t2 = Just 0,
                  t3 = Just 0,
                  t4 = Just 0,
                  t5 = Just 0,
                  time = Just 24.34,
                  rawPoints = Just 133,
                  totalPoints = Just 123,
                  hitFactor = Just 5.0534,
                  stagePoints = Just 106.8501,
                  stagePlace = Just 2,
                  stagePowerFactor = Nothing
                }
            ]
        )

    it "scoreLinesWithFieldName" $ do
      shouldBe
        (runParser scoresWithFieldName mempty (header <> "\n" <> scoreLines <> "\n"))
        ( Right
            [ [ ("Gun", "Pistol"),
                ("Stage", "1"),
                ("Comp", "1"),
                ("DQ", "No"),
                ("DNF", "No"),
                ("A", "26"),
                ("B", "0"),
                ("C", "2"),
                ("D", "0"),
                ("Miss", "0"),
                ("No Shoot", "0"),
                ("Procedural", "0"),
                ("Double Poppers", "20"),
                ("Double Popper Miss", "0"),
                ("Late Shot", "0"),
                ("Extra Shot", "0"),
                ("Extra Hit", "0"),
                ("No Penalty Miss", "0"),
                ("Additional Penalty", "0"),
                ("Total Penalty", "0"),
                ("T1", "20.54"),
                ("T2", "0"),
                ("T3", "0"),
                ("T4", "0"),
                ("T5", "0"),
                ("Time", "20.54"),
                ("Raw Points", "136"),
                ("Total Points", "136"),
                ("Hit Factor", "6.6212"),
                ("Stage Points", "140.0000"),
                ("Stage Place", "1"),
                ("Stage Power Factor", "")
              ],
              [ ("Gun", "Pistol"),
                ("Stage", "1"),
                ("Comp", "2"),
                ("DQ", "No"),
                ("DNF", "No"),
                ("A", "26"),
                ("B", "0"),
                ("C", "1"),
                ("D", "0"),
                ("Miss", "1"),
                ("No Shoot", "0"),
                ("Procedural", "0"),
                ("Double Poppers", "20"),
                ("Double Popper Miss", "0"),
                ("Late Shot", "0"),
                ("Extra Shot", "0"),
                ("Extra Hit", "0"),
                ("No Penalty Miss", "0"),
                ("Additional Penalty", "0"),
                ("Total Penalty", "10"),
                ("T1", "24.34"),
                ("T2", "0"),
                ("T3", "0"),
                ("T4", "0"),
                ("T5", "0"),
                ("Time", "24.34"),
                ("Raw Points", "133"),
                ("Total Points", "123"),
                ("Hit Factor", "5.0534"),
                ("Stage Points", "106.8501"),
                ("Stage Place", "2"),
                ("Stage Power Factor", "")
              ]
            ]
        )

    it "header" $ do
      shouldBe
        (runParser scoreHeader mempty (header <> "\n"))
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

testTree :: IO TestTree
testTree = testSpec "Parser.Score" $ do
  scoreParserSpec
