{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Practiscore.Parser.Score
  ( Score (..),
    scoreHeader,
    scoreHeaderIdentifier,
    scoresWithFieldName,
    scoreLine,
    emptyScore,
    scoreWithFieldNames,
    decodeScore,
  )
where

import Conduit (ConduitT)
import Conduit qualified
import Data.Conduit.Lift (evalStateC)
import Practiscore.Parser (Parser, cells, lineStartingWith)
import Practiscore.USPSA (CompId (..))
import Text.Megaparsec (eof)

data Score = Score
  { gun :: Text,
    stage :: Maybe Word8,
    comp :: Maybe CompId,
    dQ :: Text,
    dNF :: Text,
    a :: Word8,
    b :: Word8,
    c :: Word8,
    d :: Word8,
    miss :: Word8,
    noShoot :: Word8,
    procedural :: Word8,
    doublePoppers :: Word8,
    doublePopperMiss :: Word8,
    lateShot :: Word8,
    extraShot :: Word8,
    extraHit :: Word8,
    noPenaltyMiss :: Word8,
    additionalPenalty :: Word8,
    totalPenalty :: Word8,
    t1 :: Maybe Word8,
    t2 :: Maybe Word8,
    t3 :: Maybe Word8,
    t4 :: Maybe Word8,
    t5 :: Maybe Word8,
    time :: Maybe Double,
    rawPoints :: Maybe Word8,
    totalPoints :: Maybe Word8,
    hitFactor :: Maybe Double,
    stagePoints :: Maybe Double,
    stagePlace :: Maybe Word8,
    stagePowerFactor :: Maybe Double
  }
  deriving stock (Show, Eq)

emptyScore :: Score
emptyScore =
  Score
    { gun = "",
      stage = Nothing,
      comp = Nothing,
      dQ = "",
      dNF = "",
      a = 0,
      b = 0,
      c = 0,
      d = 0,
      miss = 0,
      noShoot = 0,
      procedural = 0,
      doublePoppers = 0,
      doublePopperMiss = 0,
      lateShot = 0,
      extraShot = 0,
      extraHit = 0,
      noPenaltyMiss = 0,
      additionalPenalty = 0,
      totalPenalty = 0,
      t1 = Nothing,
      t2 = Nothing,
      t3 = Nothing,
      t4 = Nothing,
      t5 = Nothing,
      time = Nothing,
      rawPoints = Nothing,
      hitFactor = Nothing,
      totalPoints = Nothing,
      stagePoints = Nothing,
      stagePlace = Nothing,
      stagePowerFactor = Nothing
    }

scoresWithFieldName :: Parser [[(String, String)]]
scoresWithFieldName = do
  header <- scoreHeader
  lines <- scoreLines
  pure $
    lines <&> \line ->
      zipWith (\h l -> (h, l)) header line

scoreLines :: Parser [[String]]
scoreLines = many scoreLine

scoreLine :: Parser [String]
scoreLine = scoreLineIdentifier *> cells <* eof

scoreLineIdentifier :: Parser ()
scoreLineIdentifier = lineStartingWith "I "

scoreHeader :: Parser [String]
scoreHeader = scoreHeaderIdentifier *> cells <* eof

scoreHeaderIdentifier :: Parser ()
scoreHeaderIdentifier = lineStartingWith "H "

scoreWithFieldNames :: (Monad m) => ConduitT ([String], [String]) [(String, String)] m ()
scoreWithFieldNames = Conduit.concatMapAccumC scoreStepWithFieldNames [[]]
  where
    scoreStepWithFieldNames :: ([String], [String]) -> [[(String, String)]] -> ([[(String, String)]], [[(String, String)]])
    scoreStepWithFieldNames (header, line) accum
      | not (null header),
        not (null line) =
          (accum, [zipWith (\h l -> (h, l)) header line])
    scoreStepWithFieldNames _ accum = (accum, [])

decodeScore :: (Monad m) => ConduitT [(String, String)] Score m ()
decodeScore = evalStateC emptyScore $ Conduit.awaitForever $ \keyValPair -> do
  for_ keyValPair $ \(header, val) ->
    case header of
      "Gun" -> modify (\score -> score {gun = toText val})
      "Stage" -> modify (\score -> score {stage = readMaybe val})
      "Comp" ->
        modify
          ( \score ->
              let Score {..} = score
               in Score {comp = fmap CompId $ readMaybe (toString val), ..}
          )
      "DQ" -> modify (\score -> score {dQ = toText val})
      "DNF" -> modify (\score -> score {dNF = toText val})
      "A" -> modify (\score -> score {a = fromMaybe 0 $ readMaybe val})
      "B" -> modify (\score -> score {b = fromMaybe 0 $ readMaybe val})
      "C" -> modify (\score -> score {c = fromMaybe 0 $ readMaybe val})
      "D" -> modify (\score -> score {d = fromMaybe 0 $ readMaybe val})
      "Miss" -> modify (\score -> score {miss = fromMaybe 0 $ readMaybe val})
      "No Shoot" -> modify (\score -> score {noShoot = fromMaybe 0 $ readMaybe val})
      "Procedural" -> modify (\score -> score {procedural = fromMaybe 0 $ readMaybe val})
      "Double Poppers" -> modify (\score -> score {doublePopperMiss = fromMaybe 0 $ readMaybe val})
      "Double Popper Miss" -> modify (\score -> score {doublePopperMiss = fromMaybe 0 $ readMaybe val})
      "Late Shot" -> modify (\score -> score {lateShot = fromMaybe 0 $ readMaybe val})
      "Extra Shot" -> modify (\score -> score {extraShot = fromMaybe 0 $ readMaybe val})
      "Extra Hit" -> modify (\score -> score {extraHit = fromMaybe 0 $ readMaybe val})
      "No Penalty Miss" -> modify (\score -> score {noPenaltyMiss = fromMaybe 0 $ readMaybe val})
      "Additional Penalty" -> modify (\score -> score {additionalPenalty = fromMaybe 0 $ readMaybe val})
      "Total Penalty" -> modify (\score -> score {totalPenalty = fromMaybe 0 $ readMaybe val})
      "T1" -> modify (\score -> score {t1 = readMaybe val})
      "T2" -> modify (\score -> score {t2 = readMaybe val})
      "T3" -> modify (\score -> score {t3 = readMaybe val})
      "T4" -> modify (\score -> score {t4 = readMaybe val})
      "T5" -> modify (\score -> score {t5 = readMaybe val})
      "Time" -> modify (\score -> score {time = readMaybe val})
      "Raw Points" -> modify (\score -> score {rawPoints = readMaybe val})
      "Total Points" -> modify (\score -> score {totalPoints = readMaybe val})
      "Hit Factor" -> modify (\score -> score {hitFactor = readMaybe val})
      "Stage Points" -> modify (\score -> score {stagePoints = readMaybe val})
      "Stage Place" -> modify (\score -> score {stagePlace = readMaybe val})
      "Stage Power Factor" -> modify (\score -> score {stagePowerFactor = readMaybe val})
      _ -> pure ()
  score <- get
  Conduit.yield score
