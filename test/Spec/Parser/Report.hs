{-# language ScopedTypeVariables #-}
module Spec.Parser.Report (testTree) where

import Practiscore.Parser.Report
  ( ReportFields (..),
    info,
    matchSummary,
    reportFields,
    title,
    zMetadata,
  )
import Practiscore.Parser.Shooter (Shooter(..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (runParser)
import Practiscore.USPSA (UspsaMemberId(..), CompId(..))

reportParserSpec :: ByteString -> ByteString -> Spec
reportParserSpec shooterLines scoreLines = do
  describe "ReportParser" $ do
    let titleLine = "$PRACTISCORE 1.682 RESULTS\n"
    let infoLines = "$INFO Region:USPSA\n$INFO PLATFORM IOS\n$INFO PractiScore_Product: PractiScore (iOS)\n$INFO PractiScore_Version:1.682\n$INFO Match name:LPRGC - Lower Providence USPSA Match November 2025  Match\n$INFO Match date:11/01/2025\n$INFO Club Name:LPRGC\n$INFO Club Code:MID04\n$INFO Multigun:0\n$INFO Scoring:0\n$INFO Classifiers:1\n$INFO Match Level: Level I\n"
    let zline = "Z 3.0\n"
    let summaryLine = "A LPRGC - Lower Providence USPSA Match November 2025  Match,LPRGC,11/01/2025\n"

    it "title" $ do
      shouldBe
        (runParser title mempty titleLine)
        (Right "1.682 RESULTS")
    it "info" $ do
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
        (runParser zMetadata mempty zline)
        (Right "3.0")

    it "matchSummary" $
      shouldBe
        (runParser matchSummary mempty summaryLine)
        (Right "LPRGC - Lower Providence USPSA Match November 2025  Match,LPRGC,11/01/2025")

    it "reportFields - title" $
      shouldBe
        (runParser reportFields mempty titleLine)
        (Right (Title "1.682 RESULTS"))

    it "reportFields - info" $
      shouldBe
        (runParser reportFields mempty infoLines)
        ( Right
            ( InfoMetadata
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
        )

    it "reportFields - zMetadata" $
      shouldBe
        (runParser reportFields mempty zline)
        (Right (ZMetadata "3.0"))

    it "reportFields - summary" $
      shouldBe
        (runParser reportFields mempty summaryLine)
        (Right (Summary "LPRGC - Lower Providence USPSA Match November 2025  Match,LPRGC,11/01/2025"))

    it "reportFields - shooters" $
      shouldBe
        (runParser reportFields mempty (decodeUtf8 shooterLines))
        ( Right
            ( Shooters
                [ Shooter
                    { comp = Just (CompId {unCompId = 1}),
                      uspsa = Just (UspsaMemberId { unUspsaMemberId = "A163922" }),
                      firstname = "first-name-1",
                      lastname = "last-name-1",
                      dqpistol = "No",
                      dqrifle = "No",
                      dqshotgun = "No",
                      reentry = "No",
                      class_ = "B",
                      division = "Carry Optics",
                      matchPoints = "568.8844",
                      placeOverall = "1",
                      powerFactor = "Minor",
                      shotgunDivision = "Open",
                      shotgunPowerFactor = "Major",
                      shotgunPlaceOverall = "",
                      shotgunEntered = "No",
                      shotgunMatchPoints = "",
                      rifleDivision = "Open",
                      riflePowerFactor = "Minor",
                      riflePlaceOverall = "",
                      rifleEntered = "No",
                      rifleMatchPoints = "",
                      aggregate = "No",
                      aggregateDivision = "",
                      aggregatePistolPercent = "",
                      aggregatePistolPoints = "",
                      aggregatePlace = "",
                      aggregateRiflePercent = "",
                      aggregateRiflePoints = "",
                      aggregateShotgunPercent = "",
                      aggregateShotgunPoints = "",
                      aggregateTotal = "",
                      female = "No",
                      age = "",
                      law = "No",
                      military = "No"
                    },
                  Shooter
                    { comp = Just (CompId {unCompId = 2}),
                      uspsa = Nothing,
                      firstname = "first-name-2",
                      lastname = "last-name-2",
                      dqpistol = "No",
                      dqrifle = "No",
                      dqshotgun = "No",
                      reentry = "No",
                      class_ = "",
                      division = "Carry Optics",
                      matchPoints = "467.1983",
                      placeOverall = "2",
                      powerFactor = "Minor",
                      shotgunDivision = "Open",
                      shotgunPowerFactor = "Major",
                      shotgunPlaceOverall = "",
                      shotgunEntered = "No",
                      shotgunMatchPoints = "",
                      rifleDivision = "Open",
                      riflePowerFactor = "Minor",
                      riflePlaceOverall = "",
                      rifleEntered = "No",
                      rifleMatchPoints = "",
                      aggregate = "No",
                      aggregateDivision = "",
                      aggregatePistolPercent = "",
                      aggregatePistolPoints = "",
                      aggregatePlace = "",
                      aggregateRiflePercent = "",
                      aggregateRiflePoints = "",
                      aggregateShotgunPercent = "",
                      aggregateShotgunPoints = "",
                      aggregateTotal = "",
                      female = "No",
                      age = "",
                      law = "No",
                      military = "No"
                    }
                ]
            )
        )
    it "reportFields - scores" $
      case (runParser reportFields mempty (decodeUtf8 scoreLines)) of
        Left err -> error (show err)
        Right fields -> 
          case fields of
            Scores scores -> shouldNotBe (length scores) 0
            _ -> error "did not match scores"

testTree :: IO TestTree
testTree = do
  scoreLines <- readFileBS "./test/Spec/Parser/scores.csv"
  shooterLines <- readFileBS "./test/Spec/Parser/shooters.csv"
  testSpec "Parser.Report" $ do
    reportParserSpec shooterLines scoreLines
