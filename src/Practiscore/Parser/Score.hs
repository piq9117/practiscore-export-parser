{-# LANGUAGE DerivingStrategies #-}

module Practiscore.Parser.Score
  ( Score (..),
    scoreHeader,
    scoreHeaderIdentifier,
    scoresWithFieldName,
    decodeScores,
  )
where

import Practiscore.Parser (Parser, cells, lineStartingWith)
import Practiscore.USPSA (CompId (..))
import Text.Megaparsec.Char (newline)

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

decodeScores :: Parser [Score]
decodeScores = do
  scores <- scoresWithFieldName
  pure $
    scores <&> \score ->
      foldr
        ( \(header, cell) accum ->
            case header of
              "Gun" -> accum {gun = toText cell}
              "Stage" -> accum {stage = readMaybe (toString cell)}
              "Comp" -> accum {comp = fmap CompId $ readMaybe (toString cell)}
              "DQ" -> accum {dQ = toText cell}
              "DNF" -> accum {dNF = toText cell}
              "A" -> accum {a = fromMaybe 0 $ readMaybe (toString cell)}
              "B" -> accum {b = fromMaybe 0 $ readMaybe (toString cell)}
              "C" -> accum {c = fromMaybe 0 $ readMaybe (toString cell)}
              "D" -> accum {d = fromMaybe 0 $ readMaybe (toString cell)}
              "Miss" -> accum {miss = fromMaybe 0 $ readMaybe (toString cell)}
              "No Shoot" -> accum {noShoot = fromMaybe 0 $ readMaybe (toString cell)}
              "Procedural" -> accum {procedural = fromMaybe 0 $ readMaybe (toString cell)}
              "Double Poppers" -> accum {doublePoppers = fromMaybe 0 $ readMaybe (toString cell)}
              "Double Popper Miss" -> accum {doublePopperMiss = fromMaybe 0 $ readMaybe (toString cell)}
              "Late Shot" -> accum {lateShot = fromMaybe 0 $ readMaybe (toString cell)}
              "Extra Shot" -> accum {extraShot = fromMaybe 0 $ readMaybe (toString cell)}
              "Extra Hit" -> accum {extraHit = fromMaybe 0 $ readMaybe (toString cell)}
              "No Penalty Miss" -> accum {noPenaltyMiss = fromMaybe 0 $ readMaybe (toString cell)}
              "Additional Penalty" -> accum {additionalPenalty = fromMaybe 0 $ readMaybe (toString cell)}
              "Total Penalty" -> accum {totalPenalty = fromMaybe 0 $ readMaybe (toString cell)}
              "T1" -> accum {t1 = readMaybe (toString cell)}
              "T2" -> accum {t2 = readMaybe (toString cell)}
              "T3" -> accum {t3 = readMaybe (toString cell)}
              "T4" -> accum {t4 = readMaybe (toString cell)}
              "T5" -> accum {t5 = readMaybe (toString cell)}
              "Time" -> accum {time = readMaybe (toString cell)}
              "Raw Points" -> accum {rawPoints = readMaybe (toString cell)}
              "Total Points" -> accum {totalPoints = readMaybe (toString cell)}
              "Hit Factor" -> accum {hitFactor = readMaybe (toString cell)}
              "Stage Points" -> accum {stagePoints = readMaybe (toString cell)}
              "Stage Place" -> accum {stagePlace = readMaybe (toString cell)}
              "Stage Power Factor" -> accum {stagePowerFactor = readMaybe (toString cell)}
              _ -> accum
        )
        emptyScore
        score

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
scoreLine = scoreLineIdentifier *> cells <* newline

scoreLineIdentifier :: Parser ()
scoreLineIdentifier = lineStartingWith "I "

scoreHeader :: Parser [String]
scoreHeader = scoreHeaderIdentifier *> cells <* newline

scoreHeaderIdentifier :: Parser ()
scoreHeaderIdentifier = lineStartingWith "H "
