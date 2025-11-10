{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Parser.Report (testTree) where

import Practiscore.Parser.Report
  ( ReportFields (..),
    reportFields,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (runParser)

reportParserSpec :: Spec
reportParserSpec = do
  describe "ReportParser" $ do
    let titleLine = "$PRACTISCORE 1.682 RESULTS"
    let infoLines :: [String] =
          [ "$INFO Region:USPSA",
            "$INFO PLATFORM IOS",
            "$INFO PractiScore_Product: PractiScore (iOS)",
            "$INFO PractiScore_Version:1.682",
            "$INFO Match name:LPRGC - Lower Providence USPSA Match November 2025  Match",
            "$INFO Match date:11/01/2025",
            "$INFO Club Name:LPRGC",
            "$INFO Club Code:MID04",
            "$INFO Multigun:0",
            "$INFO Scoring:0",
            "$INFO Classifiers:1",
            "$INFO Match Level: Level I"
          ]
    let zline = "Z 3.0"
    let summaryLine = "A LPRGC - Lower Providence USPSA Match November 2025  Match,LPRGC,11/01/2025"

    it "reportFields - title" $
      shouldBe
        (runParser reportFields mempty titleLine)
        (Right (Title "1.682 RESULTS"))

    it "reportFields - info" $
      shouldBe
        (runParser reportFields mempty <$> infoLines)
        [ Right (InfoMetadata "Region:USPSA"),
          Right (InfoMetadata "PLATFORM IOS"),
          Right (InfoMetadata "PractiScore_Product: PractiScore (iOS)"),
          Right (InfoMetadata "PractiScore_Version:1.682"),
          Right (InfoMetadata "Match name:LPRGC - Lower Providence USPSA Match November 2025  Match"),
          Right (InfoMetadata "Match date:11/01/2025"),
          Right (InfoMetadata "Club Name:LPRGC"),
          Right (InfoMetadata "Club Code:MID04"),
          Right (InfoMetadata "Multigun:0"),
          Right (InfoMetadata "Scoring:0"),
          Right (InfoMetadata "Classifiers:1"),
          Right (InfoMetadata "Match Level: Level I")
        ]

    it "reportFields - zMetadata" $
      shouldBe
        (runParser reportFields mempty zline)
        (Right (ZMetadata "3.0"))

    it "reportFields - summary" $
      shouldBe
        (runParser reportFields mempty summaryLine)
        (Right (Summary "LPRGC - Lower Providence USPSA Match November 2025  Match,LPRGC,11/01/2025"))

    it "reportFields - shooterHeaderLine" $ do
      let header = "D Comp,USPSA,FirstName,LastName,DQPistol,DQRifle,DQShotgun,Reentry,Class,Division,Match Points,Place Overall,Power Factor,Shotgun Division,Shotgun Power Factor,Shotgun Place Overall,Shotgun Entered,Shotgun Match Points,Rifle Division,Rifle Power Factor,Rifle Place Overall,Rifle Entered,Rifle Match Points,Aggregate,Aggregate Division,Aggregate Pistol Percent,Aggregate Pistol Points,Aggregate Place,Aggregate Rifle Percent,Aggregate Rifle Points,Aggregate Shotgun Percent,Aggregate Shotgun Points,Aggregate Total,Female,Age,Law,Military"

      shouldBe
        (runParser reportFields mempty header)
        ( Right
            ( ShooterHeaderLine
                [ "Comp",
                  "USPSA",
                  "FirstName",
                  "LastName",
                  "DQPistol",
                  "DQRifle",
                  "DQShotgun",
                  "Reentry",
                  "Class",
                  "Division",
                  "Match Points",
                  "Place Overall",
                  "Power Factor",
                  "Shotgun Division",
                  "Shotgun Power Factor",
                  "Shotgun Place Overall",
                  "Shotgun Entered",
                  "Shotgun Match Points",
                  "Rifle Division",
                  "Rifle Power Factor",
                  "Rifle Place Overall",
                  "Rifle Entered",
                  "Rifle Match Points",
                  "Aggregate",
                  "Aggregate Division",
                  "Aggregate Pistol Percent",
                  "Aggregate Pistol Points",
                  "Aggregate Place",
                  "Aggregate Rifle Percent",
                  "Aggregate Rifle Points",
                  "Aggregate Shotgun Percent",
                  "Aggregate Shotgun Points",
                  "Aggregate Total",
                  "Female",
                  "Age",
                  "Law",
                  "Military"
                ]
            )
        )

    it "reportFields - shooter" $ do
      let shooterLines =
            [ "E 1,member-id-1,lastname-1,firstname-1,No,No,No,No,B,Carry Optics,568.8844,1,Minor,Open,Major,,No,,Open,Minor,,No,,No,,,,,,,,,,No,,No,No",
              "E 2,,lastname-2,firstname-2,No,No,No,No,,Carry Optics,467.1983,2,Minor,Open,Major,,No,,Open,Minor,,No,,No,,,,,,,,,,No,,No,No",
              "E 3,member-id-2,lastname-3,firstname-3,No,No,No,No,C,Carry Optics,455.4556,3,Minor,Open,Major,,No,,Open,Minor,,No,,No,,,,,,,,,,No,,No,No",
              "E 4,member-id-3,lastname-4,firstname-4,No,No,No,No,B,Carry Optics,454.0943,4,Minor,Open,Major,,No,,Open,Minor,,No,,No,,,,,,,,,,No,,No,No",
              "E 5,member-id-4,lastname-5,firstname-5,No,No,No,No,C,Carry Optics,448.7579,5,Minor,Open,Major,,No,,Open,Minor,,No,,No,,,,,,,,,,No,,No,No"
            ]
      shouldBe
        (runParser reportFields mempty <$> shooterLines)
        [ Right (ShooterLine ["1", "member-id-1", "lastname-1", "firstname-1", "No", "No", "No", "No", "B", "Carry Optics", "568.8844", "1", "Minor", "Open", "Major", "", "No", "", "Open", "Minor", "", "No", "", "No", "", "", "", "", "", "", "", "", "", "No", "", "No", "No"]),
          Right (ShooterLine ["2", "", "lastname-2", "firstname-2", "No", "No", "No", "No", "", "Carry Optics", "467.1983", "2", "Minor", "Open", "Major", "", "No", "", "Open", "Minor", "", "No", "", "No", "", "", "", "", "", "", "", "", "", "No", "", "No", "No"]),
          Right (ShooterLine ["3", "member-id-2", "lastname-3", "firstname-3", "No", "No", "No", "No", "C", "Carry Optics", "455.4556", "3", "Minor", "Open", "Major", "", "No", "", "Open", "Minor", "", "No", "", "No", "", "", "", "", "", "", "", "", "", "No", "", "No", "No"]),
          Right (ShooterLine ["4", "member-id-3", "lastname-4", "firstname-4", "No", "No", "No", "No", "B", "Carry Optics", "454.0943", "4", "Minor", "Open", "Major", "", "No", "", "Open", "Minor", "", "No", "", "No", "", "", "", "", "", "", "", "", "", "No", "", "No", "No"]),
          Right (ShooterLine ["5", "member-id-4", "lastname-5", "firstname-5", "No", "No", "No", "No", "C", "Carry Optics", "448.7579", "5", "Minor", "Open", "Major", "", "No", "", "Open", "Minor", "", "No", "", "No", "", "", "", "", "", "", "", "", "", "No", "", "No", "No"])
        ]

    it "reportFields - stageHeader" $ do
      let header :: String = "F Number,Guntype,Minimum Rounds,Maximum Points,Classifier,Classifier_No,Stage_name,ScoringType,TimesRun"
      shouldBe
        (runParser reportFields mempty header)
        ( Right
            ( StageHeaderLine
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
        )

    it "reportFields - stages" $ do
      let stages =
            [ "G 1,Pistol,28,140,No,,I Am A Meat Popsicle,Comstock,1",
              "G 2,Pistol,24,120,Yes,03-18,High Standards,Virginia,2",
              "G 3,Pistol,8,40,No,,Natty Boh,Comstock,1",
              "G 4,Pistol,9,45,No,,Island Hoppers,Comstock,1",
              "G 5,Pistol,18,90,No,,Amish You Were,Comstock,1",
              "G 6,Pistol,32,160,No,,Lowered Providence,Comstock,1"
            ]
      shouldBe
        (runParser reportFields mempty <$> stages)
        [ Right (StageLine ["1", "Pistol", "28", "140", "No", "", "I Am A Meat Popsicle", "Comstock", "1"]),
          Right (StageLine ["2", "Pistol", "24", "120", "Yes", "03-18", "High Standards", "Virginia", "2"]),
          Right (StageLine ["3", "Pistol", "8", "40", "No", "", "Natty Boh", "Comstock", "1"]),
          Right (StageLine ["4", "Pistol", "9", "45", "No", "", "Island Hoppers", "Comstock", "1"]),
          Right (StageLine ["5", "Pistol", "18", "90", "No", "", "Amish You Were", "Comstock", "1"]),
          Right (StageLine ["6", "Pistol", "32", "160", "No", "", "Lowered Providence", "Comstock", "1"])
        ]

    it "reportFields - scoreHeader" $ do
      let header = "H Gun,Stage,Comp,DQ,DNF,A,B,C,D,Miss,No Shoot,Procedural,Double Poppers,Double Popper Miss,Late Shot,Extra Shot,Extra Hit,No Penalty Miss,Additional Penalty,Total Penalty,T1,T2,T3,T4,T5,Time,Raw Points,Total Points,Hit Factor,Stage Points,Stage Place,Stage Power Factor"
      shouldBe
        (runParser reportFields mempty header)
        ( Right
            ( ScoreHeaderLine
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
        )

    it "reportFields - score" $ do
      let lines =
            [ "I Pistol,1,1,No,No,26,0,2,0,0,0,0,20,0,0,0,0,0,0,0,20.54,0,0,0,0,20.54,136,136,6.6212,140.0000,1,",
              "I Pistol,1,2,No,No,26,0,1,0,1,0,0,20,0,0,0,0,0,0,10,24.34,0,0,0,0,24.34,133,123,5.0534,106.8501,2,",
              "I Pistol,1,3,No,No,27,0,0,0,1,0,0,20,0,0,0,0,0,0,10,29.25,0,0,0,0,29.25,135,125,4.2735,90.3598,5,"
            ]
      shouldBe
        (runParser reportFields mempty <$> lines)
        [ Right (ScoreLine ["Pistol", "1", "1", "No", "No", "26", "0", "2", "0", "0", "0", "0", "20", "0", "0", "0", "0", "0", "0", "0", "20.54", "0", "0", "0", "0", "20.54", "136", "136", "6.6212", "140.0000", "1", ""]),
          Right (ScoreLine ["Pistol", "1", "2", "No", "No", "26", "0", "1", "0", "1", "0", "0", "20", "0", "0", "0", "0", "0", "0", "10", "24.34", "0", "0", "0", "0", "24.34", "133", "123", "5.0534", "106.8501", "2", ""]),
          Right (ScoreLine ["Pistol", "1", "3", "No", "No", "27", "0", "0", "0", "1", "0", "0", "20", "0", "0", "0", "0", "0", "0", "10", "29.25", "0", "0", "0", "0", "29.25", "135", "125", "4.2735", "90.3598", "5", ""])
        ]

testTree :: IO TestTree
testTree = do
  testSpec "Parser.Report" $ do
    reportParserSpec
