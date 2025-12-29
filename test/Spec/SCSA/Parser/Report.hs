{-# LANGUAGE DuplicateRecordFields #-}

module Spec.SCSA.Parser.Report (testTree) where

import Conduit ((.|))
import Conduit qualified
import Practiscore.SCSA.Parser.Report
  ( ReportFields (..),
    reportFieldStream,
    reportFields,
    toScoreStream,
    toShooterStream,
    toStageStream,
  )
import Practiscore.SCSA.Parser.Score (Score (..))
import Practiscore.SCSA.Parser.Shooter (Shooter (..))
import Practiscore.SCSA.Parser.Stage (Stage (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (runParser)
import Prelude hiding (id)

reportFieldsSpec :: Spec
reportFieldsSpec =
  describe "reportFields" $ do
    it "reportFields - infometadata" $ do
      let infoLine = "ER,pricetown,1,\"Pricetown Steel Challenge - December 2025 Sunday Match\",20251221,20251226,,,,,,,,,,,,,,,,,,,,"
      shouldBe
        (runParser reportFields mempty infoLine)
        (Right (InfoMetadata ["pricetown", "1", "\"Pricetown Steel Challenge - December 2025 Sunday Match\"", "20251221", "20251226", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""]))

    it "reportFields - shooterLine" $ do
      let shooterLine = "EC,1,member-id-1,firstname-1,lastname-1,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,,,,,,,,,,,,,"
      shouldBe
        (runParser reportFields mempty shooterLine)
        (Right (ShooterLine ["1", "member-id-1", "firstname-1", "lastname-1", "TRUE", "FALSE", "FALSE", "TRUE", "FALSE", "FALSE", "FALSE", "FALSE", "", "", "", "", "", "", "", "", "", "", "", "", ""]))

    it "reportFields - shooterLines" $ do
      let shooterLines =
            [ "EC,1,member-id-1,firstname-1,lastname-1,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,,,,,,,,,,,,,",
              "EC,2,member-id-2,firstname-2,lastname-2,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,,,,,,,,,,,,,",
              "EC,3,member-id-3,firstname-3,lastname-3,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,,,,,,,,,,,,,",
              "EC,4,member-id-4,firstname-4,lastname-4,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,,,,,,,,,,,,,"
            ]
      shouldBe
        (runParser reportFields mempty <$> shooterLines)
        [ Right
            ( ShooterLine
                [ "1",
                  "member-id-1",
                  "firstname-1",
                  "lastname-1",
                  "TRUE",
                  "FALSE",
                  "FALSE",
                  "TRUE",
                  "FALSE",
                  "FALSE",
                  "FALSE",
                  "FALSE",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  ""
                ]
            ),
          Right
            ( ShooterLine
                [ "2",
                  "member-id-2",
                  "firstname-2",
                  "lastname-2",
                  "FALSE",
                  "TRUE",
                  "FALSE",
                  "FALSE",
                  "FALSE",
                  "FALSE",
                  "FALSE",
                  "FALSE",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  ""
                ]
            ),
          Right
            ( ShooterLine
                [ "3",
                  "member-id-3",
                  "firstname-3",
                  "lastname-3",
                  "FALSE",
                  "FALSE",
                  "FALSE",
                  "TRUE",
                  "FALSE",
                  "FALSE",
                  "FALSE",
                  "FALSE",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  ""
                ]
            ),
          Right
            ( ShooterLine
                [ "4",
                  "member-id-4",
                  "firstname-4",
                  "lastname-4",
                  "TRUE",
                  "FALSE",
                  "FALSE",
                  "TRUE",
                  "FALSE",
                  "FALSE",
                  "FALSE",
                  "FALSE",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  ""
                ]
            )
        ]

    it "reportFields - stageLine" $ do
      let stageLine = "ST,1,\"5 To Go\",SC-101,1,1,5,TRUE,FALSE,0.00,3.00,,,,,,,,,,,,,,,"
      shouldBe
        (runParser reportFields mempty stageLine)
        (Right (StageLine ["1", "\"5 To Go\"", "SC-101", "1", "1", "5", "TRUE", "FALSE", "0.00", "3.00", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""]))

    it "reportFields - scoreLine" $ do
      let scoreLine = "SS,1,1,2,14.24,4.28,0,FALSE,4.28,0,FALSE,3.22,0,FALSE,3.2,0,FALSE,3.54,0,FALSE,,,,,,"
      shouldBe
        (runParser reportFields mempty scoreLine)
        (Right (ScoreLine ["1", "1", "2", "14.24", "4.28", "0", "FALSE", "4.28", "0", "FALSE", "3.22", "0", "FALSE", "3.2", "0", "FALSE", "3.54", "0", "FALSE", "", "", "", "", "", ""]))

    it "reportFieldStream - stage" $ do
      let fileContent = "ST,1,\"5 To Go\",SC-101,1,1,5,TRUE,FALSE,0.00,3.00,,,,,,,,,,,,,,,"
      result <-
        Conduit.runConduit $
          Conduit.yield fileContent
            .| reportFieldStream
            .| toStageStream
            .| Conduit.sinkList
      shouldBe result [Stage {id = 1, name = "5 To Go", classifierCode = "SC-101"}]

    it "reportFieldStream - shooter" $ do
      let fileContent = "EC,1,member-id-1,firstname-1,lastname-1,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,,,,,,,,,,,,,"
      result <-
        Conduit.runConduit $
          Conduit.yield fileContent
            .| reportFieldStream
            .| toShooterStream
            .| Conduit.sinkList
      shouldBe result [Shooter {id = 1, memberId = "member-id-1", firstname = Just "firstname-1", lastname = Just "lastname-1"}]

    it "reportFields - score" $ do
      let fileContents =
            [ "SS,1,1,1,14.46,3.16,0,FALSE,4.68,0,FALSE,4.32,0,FALSE,3.09,0,FALSE,3.89,0,FALSE,,,,,,",
              "SS,1,2,1,12.51,3.65,0,FALSE,6.67,1,FALSE,3.03,0,FALSE,2.81,0,FALSE,3.02,0,FALSE,,,,,,",
              "SS,1,3,1,13.11,4.26,0,FALSE,3.83,0,FALSE,3.75,0,FALSE,2.56,0,FALSE,2.97,0,FALSE,,,,,,",
              "SS,1,4,1,13.29,3.84,0,FALSE,4.13,0,FALSE,3.44,0,FALSE,3.15,0,FALSE,2.86,0,FALSE,,,,,,",
              "SS,1,5,1,14.73,3.68,0,FALSE,4.82,0,FALSE,4.22,0,FALSE,3.68,0,FALSE,3.15,0,FALSE,,,,,,",
              "SS,1,6,1,9.57,2.79,0,FALSE,2.92,0,FALSE,2.19,0,FALSE,2.02,0,FALSE,2.57,0,FALSE,,,,,,"
            ]

      result <-
        Conduit.runConduit $
          Conduit.yieldMany fileContents
            .| reportFieldStream
            .| toScoreStream
            .| Conduit.sinkList
      shouldBe
        result
        [ Score {matchTypeId = 1, stageNumber = 1, shooterId = 1, stageTotalTime = 14.46, string1Time = 3.16, string1Penalty = 0.0, string1DNF = False, string2Time = 4.68, string2Penalty = 0.0, string2DNF = False, string3Time = 4.32, string3Penalty = 0.0, string3DNF = False, string4Time = 3.09, string4Penalty = 0.0, string4DNF = False, string5Time = 3.89, string5Penalty = 0.0, string5DNF = False},
          Score {matchTypeId = 1, stageNumber = 2, shooterId = 1, stageTotalTime = 12.51, string1Time = 3.65, string1Penalty = 0.0, string1DNF = False, string2Time = 6.67, string2Penalty = 1.0, string2DNF = False, string3Time = 3.03, string3Penalty = 0.0, string3DNF = False, string4Time = 2.81, string4Penalty = 0.0, string4DNF = False, string5Time = 3.02, string5Penalty = 0.0, string5DNF = False},
          Score {matchTypeId = 1, stageNumber = 3, shooterId = 1, stageTotalTime = 13.11, string1Time = 4.26, string1Penalty = 0.0, string1DNF = False, string2Time = 3.83, string2Penalty = 0.0, string2DNF = False, string3Time = 3.75, string3Penalty = 0.0, string3DNF = False, string4Time = 2.56, string4Penalty = 0.0, string4DNF = False, string5Time = 2.97, string5Penalty = 0.0, string5DNF = False},
          Score {matchTypeId = 1, stageNumber = 4, shooterId = 1, stageTotalTime = 13.29, string1Time = 3.84, string1Penalty = 0.0, string1DNF = False, string2Time = 4.13, string2Penalty = 0.0, string2DNF = False, string3Time = 3.44, string3Penalty = 0.0, string3DNF = False, string4Time = 3.15, string4Penalty = 0.0, string4DNF = False, string5Time = 2.86, string5Penalty = 0.0, string5DNF = False},
          Score {matchTypeId = 1, stageNumber = 5, shooterId = 1, stageTotalTime = 14.73, string1Time = 3.68, string1Penalty = 0.0, string1DNF = False, string2Time = 4.82, string2Penalty = 0.0, string2DNF = False, string3Time = 4.22, string3Penalty = 0.0, string3DNF = False, string4Time = 3.68, string4Penalty = 0.0, string4DNF = False, string5Time = 3.15, string5Penalty = 0.0, string5DNF = False},
          Score {matchTypeId = 1, stageNumber = 6, shooterId = 1, stageTotalTime = 9.57, string1Time = 2.79, string1Penalty = 0.0, string1DNF = False, string2Time = 2.92, string2Penalty = 0.0, string2DNF = False, string3Time = 2.19, string3Penalty = 0.0, string3DNF = False, string4Time = 2.02, string4Penalty = 0.0, string4DNF = False, string5Time = 2.57, string5Penalty = 0.0, string5DNF = False}
        ]

testTree :: IO TestTree
testTree =
  testSpec "Spec.SCSA.Parser.Report" $
    reportFieldsSpec
