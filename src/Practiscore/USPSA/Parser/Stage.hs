module Practiscore.USPSA.Parser.Stage
  ( StageInfo (..),
    stageHeaderIdentifier,
    stageHeader,
    stageLines,
    stagesWithFieldName,
    stageLineIdentifier,
    stageHeaderLine,
    stageLine,
    decodeStageInfo,
    stageWithFieldNames,
  )
where

import Conduit (ConduitT)
import Conduit qualified
import Data.Conduit.Lift (evalStateC)
import Data.Text qualified
import Practiscore.Parser (Parser, cells, lineStartingWith)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (newline)

data StageInfo = StageInfo
  { number :: Word8,
    name :: Text,
    classifier :: Bool,
    classifierNumber :: Maybe Text
  }
  deriving (Show, Eq)

emptyStageInfo :: StageInfo
emptyStageInfo =
  StageInfo
    { number = 0,
      name = mempty,
      classifier = False,
      classifierNumber = Nothing
    }

decodeStageInfo :: (Monad m) => ConduitT [(Text, Text)] StageInfo m ()
decodeStageInfo = evalStateC emptyStageInfo $ Conduit.awaitForever $ \keyValPair -> do
  for_ keyValPair $ \(header, val) ->
    case header of
      "Number" ->
        modify (\stageInfo -> stageInfo {number = fromMaybe 0 $ readMaybe $ toString val})
      "Stage_name" ->
        modify (\stageInfo -> stageInfo {name = val})
      "Classifier" ->
        modify (\stageInfo -> stageInfo {classifier = val == "Yes"})
      "Classifier_No" ->
        modify
          ( \stageInfo ->
              stageInfo
                { classifierNumber =
                    if Data.Text.null val
                      then Nothing
                      else Just (toText val)
                }
          )
      _ -> pure ()
  stageInfo <- get
  Conduit.yield stageInfo

stageHeader :: Parser [String]
stageHeader =
  stageHeaderIdentifier *> cells <* newline

stageHeaderLine :: Parser [String]
stageHeaderLine =
  stageHeaderIdentifier *> cells <* eof

stageHeaderIdentifier :: Parser ()
stageHeaderIdentifier = lineStartingWith "F "

stageWithFieldNames :: (Monad m) => ConduitT ([Text], [Text]) [(Text, Text)] m ()
stageWithFieldNames = Conduit.concatMapAccumC stepWithFieldNames [[]]
  where
    stepWithFieldNames :: ([Text], [Text]) -> [[(Text, Text)]] -> ([[(Text, Text)]], [[(Text, Text)]])
    stepWithFieldNames (header, line) accum
      | not (null header),
        not (null line) =
          (accum, [zipWith (\h l -> (h, l)) header line])
    stepWithFieldNames _ accum = (accum, [])

stagesWithFieldName :: Parser [[(String, String)]]
stagesWithFieldName = do
  header <- stageHeader
  lines <- stageLines
  pure $
    lines <&> \line ->
      zipWith (\h l -> (h, l)) header line

stageLines :: Parser [[String]]
stageLines = many stageLine

stageLine :: Parser [String]
stageLine = stageLineIdentifier *> cells <* eof

stageLineIdentifier :: Parser ()
stageLineIdentifier = lineStartingWith "G "
