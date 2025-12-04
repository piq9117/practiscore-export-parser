{-# LANGUAGE DuplicateRecordFields #-}

module Spec.USPSA.Match (testTree) where

import Practiscore.Parser.Score (Score (..))
import Practiscore.Parser.Shooter (Shooter (..))
import Practiscore.USPSA (CompId (..), UspsaMemberId (..))
import Practiscore.USPSA.Match (Stage (..), encodeMatch)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

matchToCsvSpec :: Spec
matchToCsvSpec =
  describe "Match" $ do
    it "encode Match to csv" $ do
      let match =
            [ Stage
                { matchName = "test-match",
                  matchDate = "11/01/2025",
                  shooter =
                    Shooter
                      { comp = Just CompId {unCompId = 1},
                        uspsa = Just UspsaMemberId {unUspsaMemberId = "uspsa-member-number"},
                        firstname = "first-name",
                        lastname = "last-name",
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
                  score =
                    Score
                      { gun = "Pistol",
                        stage = 1,
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
                      }
                },
              Stage
                { matchName = "test-match",
                  matchDate = "11/01/2025",
                  shooter =
                    Shooter
                      { comp = Just CompId {unCompId = 1},
                        uspsa = Just UspsaMemberId {unUspsaMemberId = "uspsa-member-number"},
                        firstname = "first-name",
                        lastname = "last-name",
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
                  score =
                    Score
                      { gun = "Pistol",
                        stage = 1,
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
                }
            ]

      shouldBe
        (encodeMatch match)
        "uspsa_id,firstname,lastname,class,division,match_points,place_overall,gun,stage,A,B,C,D,miss,no_shoot,procedural,time,raw_points,total_points,hit_factor,stage_place,match_name,match_date\r\nuspsa-member-number,first-name,last-name,B,Carry Optics,568.8844,1,Pistol,1,26,0,2,0,0,0,0,20.54,136,136,6.6212,1,test-match,11/01/2025\r\nuspsa-member-number,first-name,last-name,B,Carry Optics,568.8844,1,Pistol,1,26,0,1,0,1,0,0,24.34,133,123,5.0534,2,test-match,11/01/2025\r\n"

testTree :: IO TestTree
testTree = testSpec "USPSA.Match" $ do
  matchToCsvSpec
