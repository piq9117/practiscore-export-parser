{-# LANGUAGE DerivingStrategies #-}

module Practiscore.SCSA.Parser.Score
  ( Score (..),
    scoreLine,
    decodeScore,
    emptyScore,
  )
where

import Conduit (ConduitT, (.|))
import Conduit qualified
import Data.Conduit.Lift (evalStateC)
import Data.Text qualified
import Practiscore.Parser (Parser, cells, lineStartingWith)
import Text.Megaparsec (eof)

data Score = Score
  { matchTypeId :: Word16,
    stageNumber :: Word16,
    shooterId :: Word16,
    stageTotalTime :: Double,
    -- string 1
    string1Time :: Double,
    string1Penalty :: Double,
    string1DNF :: Bool,
    -- string 2
    string2Time :: Double,
    string2Penalty :: Double,
    string2DNF :: Bool,
    -- string 3
    string3Time :: Double,
    string3Penalty :: Double,
    string3DNF :: Bool,
    -- string 4
    string4Time :: Double,
    string4Penalty :: Double,
    string4DNF :: Bool,
    -- string 5
    string5Time :: Double,
    string5Penalty :: Double,
    string5DNF :: Bool
  }
  deriving stock (Show, Eq)

-- match_type | stage_num | shooter_id | stage_total | str1_time | str1_pen | str1_DNF | str2_time | str2_pen | str2_DNF | str3_time | str3_pen | str3_DNF | str4_time | str4_pen | str4_DNF | str5_time | str5_pen |str5_DNF
-- 1          | 1         | 1          | 14.46       |3.16       |0         |FALSE     |4.68       |0         |FALSE     |4.32       |0         |FALSE     |3.09       |0         |FALSE     | 3.89      |0         |FALSE

decodeScore :: (Monad m) => ConduitT [Text] Score m ()
decodeScore =
  zipScoreWithHeaders
    .| ( evalStateC emptyScore $
           Conduit.awaitForever
             ( \keyValPairs -> do
                 for_ keyValPairs $ \(header, val) ->
                   case header of
                     "match type id" ->
                       modify (\score -> score {matchTypeId = fromMaybe 0 $ readMaybe $ toString val})
                     "stage number" -> modify (\score -> score {stageNumber = fromMaybe 0 $ readMaybe $ toString val})
                     "shooter id" -> modify (\score -> score {shooterId = fromMaybe 0 $ readMaybe $ toString val})
                     "stage total" -> modify (\score -> score {stageTotalTime = fromMaybe 0.0 $ readMaybe $ toString val})
                     -- string 1
                     "string1 time" -> modify (\score -> score {string1Time = fromMaybe 0.0 $ readMaybe $ toString val})
                     "string1 penalty" -> modify (\score -> score {string1Penalty = fromMaybe 0.0 $ readMaybe $ toString val})
                     "string1 dnf" -> modify (\score -> score {string1DNF = decodeBool val})
                     -- string 2
                     "string2 time" -> modify (\score -> score {string2Time = fromMaybe 0.0 $ readMaybe $ toString val})
                     "string2 penalty" -> modify (\score -> score {string2Penalty = fromMaybe 0.0 $ readMaybe $ toString val})
                     "string2 dnf" -> modify (\score -> score {string2DNF = decodeBool val})
                     -- string 3
                     "string3 time" -> modify (\score -> score {string3Time = fromMaybe 0.0 $ readMaybe $ toString val})
                     "string3 penalty" -> modify (\score -> score {string3Penalty = fromMaybe 0.0 $ readMaybe $ toString val})
                     "string3 dnf" -> modify (\score -> score {string3DNF = decodeBool val})
                     -- string 4
                     "string4 time" -> modify (\score -> score {string4Time = fromMaybe 0.0 $ readMaybe $ toString val})
                     "string4 penalty" -> modify (\score -> score {string4Penalty = fromMaybe 0.0 $ readMaybe $ toString val})
                     "string4 dnf" -> modify (\score -> score {string4DNF = decodeBool val})
                     -- string 5
                     "string5 time" -> modify (\score -> score {string5Time = fromMaybe 0.0 $ readMaybe $ toString val})
                     "string5 penalty" -> modify (\score -> score {string5Penalty = fromMaybe 0.0 $ readMaybe $ toString val})
                     "string5 dnf" -> modify (\score -> score {string5DNF = decodeBool val})
                     _ -> pure ()
                 score <- get
                 Conduit.yield score
             )
       )
  where
    decodeBool :: Text -> Bool
    decodeBool raw =
      case Data.Text.toLower raw of
        "false" -> False
        "true" -> True
        _ -> False

emptyScore :: Score
emptyScore =
  Score
    { matchTypeId = 0,
      stageNumber = 0,
      shooterId = 0,
      stageTotalTime = 0.0,
      string1Time = 0.0,
      string1Penalty = 0.0,
      string1DNF = False,
      string2Time = 0.0,
      string2Penalty = 0.0,
      string2DNF = False,
      string3Time = 0.0,
      string3Penalty = 0.0,
      string3DNF = False,
      string4Time = 0.0,
      string4Penalty = 0.0,
      string4DNF = False,
      string5Time = 0.0,
      string5Penalty = 0.0,
      string5DNF = False
    }

scoreHeaders :: [Text]
scoreHeaders =
  [ "match type id",
    "stage number",
    "shooter id",
    "stage total",
    -- string 1
    "string1 time",
    "string1 penalty",
    "string1 dnf",
    -- string 2
    "string2 time",
    "string2 penalty",
    "string2 dnf",
    -- string 3
    "string3 time",
    "string3 penalty",
    "string3 dnf",
    -- string 4
    "string4 time",
    "string4 penalty",
    "string4 dnf",
    -- string 5
    "string5 time",
    "string5 penalty",
    "string5 dnf"
  ]

scoreLine :: Parser [String]
scoreLine = lineStartingWith "SS," *> cells <* eof

zipScoreWithHeaders :: (Monad m) => ConduitT [Text] [(Text, Text)] m ()
zipScoreWithHeaders = Conduit.concatMapAccumC step [[]]
  where
    step :: [Text] -> [[(Text, Text)]] -> ([[(Text, Text)]], [[(Text, Text)]])
    step line accum
      | not (null line) = (accum, [zipWith (\h l -> (h, l)) scoreHeaders line])
    step _ accum = (accum, [])
