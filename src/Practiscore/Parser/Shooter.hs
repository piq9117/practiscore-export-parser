{-# LANGUAGE DerivingStrategies #-}

module Practiscore.Parser.Shooter
  ( Shooter (..),
    cell,
    cells,
    shooterLineIdentifier,
    shooterHeader,
    shooterHeaderIdentifier,
    shooterLine,
    toUspsaMemberId,
    shooterHeaderLine,
    emptyShooter,
    shooterWithFieldNames,
    decodeShooter
  )
where

import Conduit (ConduitT)
import Conduit qualified
import Data.Conduit.Lift (evalStateC)
import Data.Text qualified
import Practiscore.Parser
  ( Parser,
    cell,
    cells,
    lineStartingWith,
  )
import Practiscore.USPSA (CompId (..), UspsaMemberId (..))
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (newline)

data Shooter = Shooter
  { comp :: Maybe CompId,
    uspsa :: Maybe UspsaMemberId,
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
      uspsa = Nothing,
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

toUspsaMemberId :: Text -> Maybe UspsaMemberId
toUspsaMemberId rawMemberId =
  let memberId = Data.Text.strip rawMemberId
   in if Data.Text.null memberId
        then Nothing
        else Just $ UspsaMemberId {unUspsaMemberId = memberId}

shooterLine :: Parser [String]
shooterLine = shooterLineIdentifier *> cells <* eof

shooterHeaderIdentifier :: Parser ()
shooterHeaderIdentifier = lineStartingWith "D "

shooterHeader :: Parser [String]
shooterHeader = shooterHeaderIdentifier *> cells <* newline

shooterHeaderLine :: Parser [String]
shooterHeaderLine = shooterHeaderIdentifier *> cells <* eof

-- There is no documentation for this. From what I see from the exported data
-- E is the identifier for the shooter data.
-- TODO consider having an identifier module so this can be centralized
shooterLineIdentifier :: Parser ()
shooterLineIdentifier = lineStartingWith "E "

shooterWithFieldNames :: (Monad m) => ConduitT ([Text], [Text]) [(Text, Text)] m ()
shooterWithFieldNames = Conduit.concatMapAccumC stepWithFieldNames [[]]
  where
    stepWithFieldNames :: ([Text], [Text]) -> [[(Text, Text)]] -> ([[(Text, Text)]], [[(Text, Text)]])
    stepWithFieldNames (header, line) accum
      | not (null header),
        not (null line) =
          (accum, [zipWith (\h l -> (h, l)) header line])
    stepWithFieldNames _ accum = (accum, [])

decodeShooter :: (Monad m) => ConduitT [(Text, Text)] Shooter m ()
decodeShooter = evalStateC emptyShooter $ Conduit.awaitForever $ \headerWithValues -> do
  for_ headerWithValues $ \(header, val) ->
    case header of
      "Comp" -> modify (\shooter -> shooter {comp = fmap CompId $ readMaybe (toString val)})
      "USPSA" -> modify (\shooter -> shooter {uspsa = toUspsaMemberId $ val})
      "FirstName" -> modify (\shooter -> shooter {firstname = val})
      "LastName" -> modify (\shooter -> shooter {lastname = val})
      "DQPistol" -> modify (\shooter -> shooter {dqpistol = val})
      "DQRifle" -> modify (\shooter -> shooter {dqrifle = val})
      "DQShotgun" -> modify (\shooter -> shooter {dqshotgun = val})
      "Reentry" -> modify (\shooter -> shooter {reentry = val})
      "Class" -> modify (\shooter -> shooter {class_ = val})
      "Division" -> modify (\shooter -> shooter {division = val})
      "Match Points" -> modify (\shooter -> shooter {matchPoints = val})
      "Place Overall" -> modify (\shooter -> shooter {placeOverall = val})
      "Power Factor" -> modify (\shooter -> shooter {powerFactor = val})
      "Shotgun Division" -> modify (\shooter -> shooter {shotgunDivision = val})
      "Shotgun Power Factor" -> modify (\shooter -> shooter {shotgunPlaceOverall = val})
      "Shotgun Place Overall" -> modify (\shooter -> shooter {shotgunPlaceOverall = val})
      "Shotgun Entered" -> modify (\shooter -> shooter {shotgunEntered = val})
      "Shotgun Match Points" -> modify (\shooter -> shooter {shotgunMatchPoints = val})
      "Rifle Division" -> modify (\shooter -> shooter {rifleDivision = val})
      "Rifle Power Factor" -> modify (\shooter -> shooter {riflePowerFactor = val})
      "Rifle Place Overall" -> modify (\shooter -> shooter {riflePlaceOverall = val})
      "Rifle Entered" -> modify (\shooter -> shooter {rifleEntered = val})
      "Rifle Match Points" -> modify (\shooter -> shooter {rifleMatchPoints = val})
      "Aggregate" -> modify (\shooter -> shooter {aggregate = val})
      "Aggregate Division" -> modify (\shooter -> shooter {aggregateDivision = val})
      "Aggregate Pistol Percent" -> modify (\shooter -> shooter {aggregatePistolPercent = val})
      "Aggregate Pistol Points" -> modify (\shooter -> shooter {aggregatePistolPoints = val})
      "Aggregate Place" -> modify (\shooter -> shooter {aggregatePlace = val})
      "Aggregate Rifle Percent" -> modify (\shooter -> shooter {aggregateRiflePoints = val})
      "Aggregate Rifle Points" -> modify (\shooter -> shooter {aggregateRiflePoints = val})
      "Aggregate Shotgun Percent" -> modify (\shooter -> shooter {aggregateShotgunPercent = val})
      "Aggregate Shotgun Points" -> modify (\shooter -> shooter {aggregateShotgunPoints = val})
      "Aggregate Total" -> modify (\shooter -> shooter {aggregateTotal = val})
      "Female" -> modify (\shooter -> shooter {female = val})
      "Age" -> modify (\shooter -> shooter {age = val})
      "Law" -> modify (\shooter -> shooter {law = val})
      "Military" -> modify (\shooter -> shooter {military = val})
      _ -> pure ()
  shooter <- get
  Conduit.yield shooter
