{-# LANGUAGE ScopedTypeVariables #-}

module Spec.USPSA.Parser.Shooter (testTree) where

import Practiscore.USPSA.Parser.Shooter
  ( cell,
    cells,
    shooterHeaderLine,
    shooterLine,
    shooterLineIdentifier,
  )
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
        (runParser shooterLine mempty shooterLineData)
        (Right ["1", "uspsa-member-number", "first-name", "last-name", "No", "No", "No", "No", "B", "Carry Optics", "568.8844", "1", "Minor", "Open", "Major", "", "No", "", "Open", "Minor", "", "No", "", "No", "", "", "", "", "", "", "", "", "", "No", "", "No", "No"])

    it "header" $ do
      shouldBe
        (runParser shooterHeaderLine mempty header)
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

testTree :: IO TestTree
testTree = testSpec "Parser.Shooter" $ do
  shooterParserSpec
