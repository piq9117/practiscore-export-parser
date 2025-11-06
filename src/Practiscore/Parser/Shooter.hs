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
import Text.Megaparsec (runParser)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Error (ParseErrorBundle)

data Shooter = Shooter
  { comp :: Maybe Word8,
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

parseShooters :: Text -> Either (ParseErrorBundle Text Void) [Shooter]
parseShooters input = runParser decodeShooters mempty input

decodeShooters :: Parser [Shooter]
decodeShooters = do
  rawShooters <- shootersWithFieldName
  pure $
    rawShooters <&> \rawShooter ->
      foldr
        ( \(header, cell) accum ->
            case header of
              "Comp" -> accum {comp = readMaybe (toString cell)}
              "USPSA" -> accum {uspsa = cell}
              "FirstName" -> accum {firstname = cell}
              "LastName" -> accum {lastname = cell}
              "DQPistol" -> accum {dqpistol = cell}
              "DQRifle" -> accum {dqrifle = cell}
              "DQShotgun" -> accum {dqshotgun = cell}
              "Reentry" -> accum {reentry = cell}
              "Class" -> accum {class_ = cell}
              "Division" -> accum {division = cell}
              "Match Points" -> accum {matchPoints = cell}
              "Place Overall" -> accum {placeOverall = cell}
              "Power Factor" -> accum {powerFactor = cell}
              "Shotgun Division" -> accum {shotgunDivision = cell}
              "Shotgun Power Factor" -> accum {shotgunPowerFactor = cell}
              "Shotgun Place Overall" -> accum {shotgunPlaceOverall = cell}
              "Shotgun Entered" -> accum {shotgunEntered = cell}
              "Shotgun Match Points" -> accum {shotgunMatchPoints = cell}
              "Rifle Division" -> accum {rifleDivision = cell}
              "Rifle Power Factor" -> accum {riflePowerFactor = cell}
              "Rifle Place Overall" -> accum {riflePlaceOverall = cell}
              "Rifle Entered" -> accum {rifleEntered = cell}
              "Rifle Match Points" -> accum {rifleMatchPoints = cell}
              "Aggregate" -> accum {aggregate = cell}
              "Aggregate Division" -> accum {aggregateDivision = cell}
              "Aggregate Pistol Percent" -> accum {aggregatePistolPercent = cell}
              "Aggregate Pistol Points" -> accum {aggregatePistolPoints = cell}
              "Aggregate Place" -> accum {aggregatePlace = cell}
              "Aggregate Rifle Percent" -> accum {aggregateRiflePercent = cell}
              "Aggregate Rifle Points" -> accum {aggregateRiflePoints = cell}
              "Aggregate Shotgun Percent" -> accum {aggregateShotgunPercent = cell}
              "Aggregate Shotgun Points" -> accum {aggregateShotgunPoints = cell}
              "Aggregate Total" -> accum {aggregateTotal = cell}
              "Female" -> accum {female = cell}
              "Age" -> accum {age = cell}
              "Law" -> accum {law = cell}
              "Military" -> accum {military = cell}
              _ -> accum
        )
        emptyShooter
        rawShooter

shootersWithFieldName :: Parser [[(Text, Text)]]
shootersWithFieldName = do
  header <- shooterHeader
  lines <- shooterLines
  pure $
    lines <&> \line ->
      zipWith (\h l -> (h, l)) header line

shooterLines :: Parser [[Text]]
shooterLines = many shooterLine

shooterLine :: Parser [Text]
shooterLine = shooterLineIdentifier *> cells <* newline

shooterHeaderIdentifier :: Parser ()
shooterHeaderIdentifier = lineStartingWith "D "

shooterHeader :: Parser [Text]
shooterHeader = shooterHeaderIdentifier *> cells <* newline

-- There is no documentation for this. From what I see from the exported data
-- E is the identifier for the shooter data.
-- TODO consider having an identifier module so this can be centralized
shooterLineIdentifier :: Parser ()
shooterLineIdentifier = lineStartingWith "E "
