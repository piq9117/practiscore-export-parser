{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Parser.Shooter (testTree) where

import Practiscore.Parser.Shooter
  ( Shooter (..),
    cell,
    cells,
    parseShooters,
    shooterHeader,
    shooterLine,
    shooterLineIdentifier,
    shootersWithFieldName,
  )
import Practiscore.USPSA (CompId (..), UspsaMemberId (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (runParser)

shooterParserSpec :: Spec
shooterParserSpec =
  describe "ShooterParser" $ do
    let shooterLineData :: String = "E 1,uspsa-member-number,first-name,last-name,No,No,No,No,B,Carry Optics,568.8844,1,Minor,Open,Major,,No,,Open,Minor,,No,,No,,,,,,,,,,No,,No,No"

    let header :: String = "D Comp,USPSA,FirstName,LastName,DQPistol,DQRifle,DQShotgun,Reentry,Class,Division,Match Points,Place Overall,Power Factor,Shotgun Division,Shotgun Power Factor,Shotgun Place Overall,Shotgun Entered,Shotgun Match Points,Rifle Division,Rifle Power Factor,Rifle Place Overall,Rifle Entered,Rifle Match Points,Aggregate,Aggregate Division,Aggregate Pistol Percent,Aggregate Pistol Points,Aggregate Place,Aggregate Rifle Percent,Aggregate Rifle Points,Aggregate Shotgun Percent,Aggregate Shotgun Points,Aggregate Total,Female,Age,Law,Military"

    it "shooterLineIdentifier" $
      (runParser shooterLineIdentifier mempty "E ") `shouldBe` (Right ())
    it "cell" $
      (runParser cell mempty "USPSANUMBER123") `shouldBe` (Right "USPSANUMBER123")
    it "cells" $
      (runParser cells mempty "first name,last name,USPSANUMBER123") `shouldBe` (Right ["first name", "last name", "USPSANUMBER123"])
    it "shooterLine" $
      shouldBe
        (runParser shooterLine mempty (shooterLineData <> "\n"))
        (Right ["1", "uspsa-member-number", "first-name", "last-name", "No", "No", "No", "No", "B", "Carry Optics", "568.8844", "1", "Minor", "Open", "Major", "", "No", "", "Open", "Minor", "", "No", "", "No", "", "", "", "", "", "", "", "", "", "No", "", "No", "No"])
    it "header" $ do
      shouldBe
        (runParser shooterHeader mempty (header <> "\n"))
        ( Right
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

    it "shootersWithFieldName" $ do
      shouldBe
        (runParser shootersWithFieldName mempty (header <> "\n" <> shooterLineData <> "\n"))
        ( Right
            [ [ ("Comp", "1"),
                ("USPSA", "uspsa-member-number"),
                ("FirstName", "first-name"),
                ("LastName", "last-name"),
                ("DQPistol", "No"),
                ("DQRifle", "No"),
                ("DQShotgun", "No"),
                ("Reentry", "No"),
                ("Class", "B"),
                ("Division", "Carry Optics"),
                ("Match Points", "568.8844"),
                ("Place Overall", "1"),
                ("Power Factor", "Minor"),
                ("Shotgun Division", "Open"),
                ("Shotgun Power Factor", "Major"),
                ("Shotgun Place Overall", ""),
                ("Shotgun Entered", "No"),
                ("Shotgun Match Points", ""),
                ("Rifle Division", "Open"),
                ("Rifle Power Factor", "Minor"),
                ("Rifle Place Overall", ""),
                ("Rifle Entered", "No"),
                ("Rifle Match Points", ""),
                ("Aggregate", "No"),
                ("Aggregate Division", ""),
                ("Aggregate Pistol Percent", ""),
                ("Aggregate Pistol Points", ""),
                ("Aggregate Place", ""),
                ("Aggregate Rifle Percent", ""),
                ("Aggregate Rifle Points", ""),
                ("Aggregate Shotgun Percent", ""),
                ("Aggregate Shotgun Points", ""),
                ("Aggregate Total", ""),
                ("Female", "No"),
                ("Age", ""),
                ("Law", "No"),
                ("Military", "No")
              ]
            ]
        )
    it "shooter" $ do
      shouldBe
        (parseShooters (header <> "\n" <> shooterLineData <> "\n"))
        ( Right
            [ Shooter
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
                }
            ]
        )

testTree :: IO TestTree
testTree = testSpec "Parser.Shooter" $ do
  shooterParserSpec
