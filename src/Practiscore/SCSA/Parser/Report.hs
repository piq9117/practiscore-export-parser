{-# LANGUAGE DerivingStrategies #-}

module Practiscore.SCSA.Parser.Report
  ( ReportFields (..),
    MatchInfo (..),
    reportFields,
    reportFieldStream,
    toStageStream,
    toShooterStream,
    toScoreStream,
    toMatchInfoStream,
  )
where

import Conduit (ConduitT, MonadThrow, (.|))
import Conduit qualified
import Data.Conduit.Lift (evalStateC)
import Practiscore.Parser
  ( ParseError,
    Parser,
    cells,
    lineStartingWith,
    prettifyParseError,
  )
import Practiscore.SCSA.Parser.Division (divisionLine)
import Practiscore.SCSA.Parser.Score (Score (..), decodeScore, scoreLine)
import Practiscore.SCSA.Parser.Shooter (Shooter (..), decodeShooter, shooterLine)
import Practiscore.SCSA.Parser.Stage (Stage (..), decodeStage, stageLine)
import Text.Megaparsec (eof, runParser)

data ReportFields
  = InfoMetadata ![Text]
  | ShooterLine ![Text]
  | StageLine ![Text]
  | ScoreLine ![Text]
  | DivisionLine ![Text]
  | Empty
  deriving stock (Show, Eq)

parseReportFields :: String -> Either ParseError ReportFields
parseReportFields fileContent =
  bimap prettifyParseError identity $ runParser reportFields mempty fileContent

reportFields :: Parser ReportFields
reportFields =
  ((InfoMetadata <<< (fmap toText)) <$> infoLine)
    <|> ((ShooterLine <<< (fmap toText)) <$> shooterLine)
    <|> ((StageLine <<< (fmap toText)) <$> stageLine)
    <|> ((ScoreLine <<< (fmap toText)) <$> scoreLine)
    <|> ((DivisionLine <<< (fmap toText)) <$> divisionLine)
    <|> pure Empty

reportFieldStream :: (MonadThrow m, Monad m) => ConduitT String ReportFields m ()
reportFieldStream =
  Conduit.awaitForever
    ( \fileContent ->
        case parseReportFields fileContent of
          Left err -> Conduit.throwM err
          Right reportField -> Conduit.yield reportField
    )

data MatchInfo = MatchInfo
  { matchIdentifier :: Text,
    matchLevel :: Word16,
    matchName :: Text,
    matchDate :: Text
  }

toMatchInfoStream :: (Monad m) => ConduitT ReportFields MatchInfo m ()
toMatchInfoStream =
  Conduit.awaitForever
    ( \reportFields ->
        case reportFields of
          InfoMetadata line -> Conduit.yield line
          _ -> pure ()
    )
    .| decodeMatchInfo

decodeMatchInfo :: (Monad m) => ConduitT [Text] MatchInfo m ()
decodeMatchInfo =
  zipInforWithHeaders
    .| ( evalStateC emptyMatchInfo $
           Conduit.awaitForever
             ( \keyValPairs -> do
                 for_ keyValPairs $ \(header, val) ->
                   case header of
                     "match identifier" ->
                       modify (\info -> info {matchIdentifier = val})
                     "match level" ->
                       modify (\info -> info {matchLevel = fromMaybe 0 $ readMaybe $ toString val})
                     "match name" ->
                       modify (\info -> info {matchName = val})
                     "match date" ->
                       modify (\info -> info {matchDate = val})
                     _ -> pure ()
                 info <- get
                 Conduit.yield info
             )
       )

emptyMatchInfo :: MatchInfo
emptyMatchInfo =
  MatchInfo
    { matchIdentifier = mempty,
      matchLevel = 0,
      matchName = mempty,
      matchDate = mempty
    }

infoHeaders :: [Text]
infoHeaders = ["match identifier", "match level", "match name", "match date"]

infoLine :: Parser [String]
infoLine = lineStartingWith "ER," *> cells <* eof

zipInforWithHeaders :: (Monad m) => ConduitT [Text] [(Text, Text)] m ()
zipInforWithHeaders = Conduit.concatMapAccumC step [[]]
  where
    step :: [Text] -> [[(Text, Text)]] -> ([[(Text, Text)]], [[(Text, Text)]])
    step line accum
      | not (null line) = (accum, [zipWith (\h l -> (h, l)) infoHeaders line])
    step _ accum = (accum, [])

toStageStream :: (Monad m) => ConduitT ReportFields Stage m ()
toStageStream =
  Conduit.awaitForever
    ( \reportFields ->
        case reportFields of
          StageLine line -> Conduit.yield line
          _ -> pure ()
    )
    .| decodeStage

toShooterStream :: (Monad m) => ConduitT ReportFields Shooter m ()
toShooterStream =
  Conduit.awaitForever
    ( \reportFields ->
        case reportFields of
          ShooterLine line -> Conduit.yield line
          _ -> pure ()
    )
    .| decodeShooter

toScoreStream :: (Monad m) => ConduitT ReportFields Score m ()
toScoreStream =
  Conduit.awaitForever
    ( \reportFields ->
        case reportFields of
          ScoreLine line -> Conduit.yield line
          _ -> pure ()
    )
    .| decodeScore
