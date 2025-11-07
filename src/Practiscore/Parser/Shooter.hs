{-# LANGUAGE DerivingStrategies #-}

module Practiscore.Parser.Shooter
  ( Shooter (..),
    cell,
    cells,
    shooterLineIdentifier,
    parseShooters,
    shooterHeader,
    shooterHeaderIdentifier,
    shooterLine,
    shooterLines,
    decodeShooters,
    shootersWithFieldName,
  )
where

import Practiscore.Parser
  ( Parser,
    cell,
    cells,
    lineStartingWith,
  )
import Practiscore.USPSA (CompId (..))
import Text.Megaparsec (runParser)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Error (ParseErrorBundle)

data Shooter = Shooter
  { comp :: Maybe CompId,
    uspsa :: Text,
    firstname :: Text,
    lastname :: Text,
    dqpistol :: Text,
    dqrifle :: Text,
    dqshotgun :: Text,
    reentry :: Text,
    class_ :: Text,
    division :: Text,
    matchPoints :: Text,
    placeOverall :: Text,
    powerFactor :: Text,
    shotgunDivision :: Text,
    shotgunPowerFactor :: Text,
    shotgunPlaceOverall :: Text,
    shotgunEntered :: Text,
    shotgunMatchPoints :: Text,
    rifleDivision :: Text,
    riflePowerFactor :: Text,
    riflePlaceOverall :: Text,
    rifleEntered :: Text,
    rifleMatchPoints :: Text,
    aggregate :: Text,
    aggregateDivision :: Text,
    aggregatePistolPercent :: Text,
    aggregatePistolPoints :: Text,
    aggregatePlace :: Text,
    aggregateRiflePercent :: Text,
    aggregateRiflePoints :: Text,
    aggregateShotgunPercent :: Text,
    aggregateShotgunPoints :: Text,
    aggregateTotal :: Text,
    female :: Text,
    age :: Text,
    law :: Text,
    military :: Text
  }
  deriving stock (Show, Eq)

emptyShooter :: Shooter
emptyShooter =
  Shooter
    { comp = Nothing,
      uspsa = "",
      firstname = "",
      lastname = "",
      dqpistol = "",
      dqrifle = "",
      dqshotgun = "",
      reentry = "",
      class_ = "",
      division = "",
      matchPoints = "",
      placeOverall = "",
      powerFactor = "",
      shotgunDivision = "",
      shotgunPowerFactor = "",
      shotgunPlaceOverall = "",
      shotgunEntered = "",
      shotgunMatchPoints = "",
      rifleDivision = "",
      riflePowerFactor = "",
      riflePlaceOverall = "",
      rifleEntered = "",
      rifleMatchPoints = "",
      aggregate = "",
      aggregateDivision = "",
      aggregatePistolPercent = "",
      aggregatePistolPoints = "",
      aggregatePlace = "",
      aggregateRiflePercent = "",
      aggregateRiflePoints = "",
      aggregateShotgunPercent = "",
      aggregateShotgunPoints = "",
      aggregateTotal = "",
      female = "",
      age = "",
      law = "",
      military = ""
    }

parseShooters :: String -> Either (ParseErrorBundle String Void) [Shooter]
parseShooters input = runParser decodeShooters mempty input

decodeShooters :: Parser [Shooter]
decodeShooters = do
  rawShooters <- shootersWithFieldName
  pure $
    rawShooters <&> \rawShooter ->
      foldr
        ( \(header, cell) accum ->
            case header of
              "Comp" -> accum {comp = fmap CompId $ readMaybe (toString cell)}
              "USPSA" -> accum {uspsa = toText cell}
              "FirstName" -> accum {firstname = toText cell}
              "LastName" -> accum {lastname = toText cell}
              "DQPistol" -> accum {dqpistol = toText cell}
              "DQRifle" -> accum {dqrifle = toText cell}
              "DQShotgun" -> accum {dqshotgun = toText cell}
              "Reentry" -> accum {reentry = toText cell}
              "Class" -> accum {class_ = toText cell}
              "Division" -> accum {division = toText cell}
              "Match Points" -> accum {matchPoints = toText cell}
              "Place Overall" -> accum {placeOverall = toText cell}
              "Power Factor" -> accum {powerFactor = toText cell}
              "Shotgun Division" -> accum {shotgunDivision = toText cell}
              "Shotgun Power Factor" -> accum {shotgunPowerFactor = toText cell}
              "Shotgun Place Overall" -> accum {shotgunPlaceOverall = toText cell}
              "Shotgun Entered" -> accum {shotgunEntered = toText cell}
              "Shotgun Match Points" -> accum {shotgunMatchPoints = toText cell}
              "Rifle Division" -> accum {rifleDivision = toText cell}
              "Rifle Power Factor" -> accum {riflePowerFactor = toText cell}
              "Rifle Place Overall" -> accum {riflePlaceOverall = toText cell}
              "Rifle Entered" -> accum {rifleEntered = toText cell}
              "Rifle Match Points" -> accum {rifleMatchPoints = toText cell}
              "Aggregate" -> accum {aggregate = toText cell}
              "Aggregate Division" -> accum {aggregateDivision = toText cell}
              "Aggregate Pistol Percent" -> accum {aggregatePistolPercent = toText cell}
              "Aggregate Pistol Points" -> accum {aggregatePistolPoints = toText cell}
              "Aggregate Place" -> accum {aggregatePlace = toText cell}
              "Aggregate Rifle Percent" -> accum {aggregateRiflePercent = toText cell}
              "Aggregate Rifle Points" -> accum {aggregateRiflePoints = toText cell}
              "Aggregate Shotgun Percent" -> accum {aggregateShotgunPercent = toText cell}
              "Aggregate Shotgun Points" -> accum {aggregateShotgunPoints = toText cell}
              "Aggregate Total" -> accum {aggregateTotal = toText cell}
              "Female" -> accum {female = toText cell}
              "Age" -> accum {age = toText cell}
              "Law" -> accum {law = toText cell}
              "Military" -> accum {military = toText cell}
              _ -> accum
        )
        emptyShooter
        rawShooter

shootersWithFieldName :: Parser [[(String, String)]]
shootersWithFieldName = do
  header <- shooterHeader
  lines <- shooterLines
  pure $
    lines <&> \line ->
      zipWith (\h l -> (h, l)) header line

shooterLines :: Parser [[String]]
shooterLines = many shooterLine

shooterLine :: Parser [String]
shooterLine = shooterLineIdentifier *> cells <* newline

shooterHeaderIdentifier :: Parser ()
shooterHeaderIdentifier = lineStartingWith "D "

shooterHeader :: Parser [String]
shooterHeader = shooterHeaderIdentifier *> cells <* newline

-- There is no documentation for this. From what I see from the exported data
-- E is the identifier for the shooter data.
-- TODO consider having an identifier module so this can be centralized
shooterLineIdentifier :: Parser ()
shooterLineIdentifier = lineStartingWith "E "
