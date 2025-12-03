{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Practiscore.Parser.Report
  ( Report (..),
    ReportFields (..),
    MatchInfo (..),
    reportFields,
    title,
    info,
    infoLine,
    zMetadata,
    matchSummary,
    parseReportFields,
    toShooters,
    toScores,
    toMatchInfo,
  )
where

import Conduit (ConduitT, MonadUnliftIO, ResourceT, (.|))
import Conduit qualified
import Control.Applicative.Combinators (manyTill)
import Data.Conduit.Lift (evalStateC)
import Data.Text qualified
import Practiscore.Parser (ParseError, Parser, lineStartingWith, prettifyParseError)
import Practiscore.Parser.Score
  ( Score (..),
    decodeScore,
    scoreHeader,
    scoreLine,
    scoreWithFieldNames,
  )
import Practiscore.Parser.Shooter
  ( Shooter (..),
    decodeShooter,
    shooterHeaderLine,
    shooterLine,
    shooterWithFieldNames,
  )
import Practiscore.Parser.Stage (stageHeaderLine, stageLine)
import Text.Megaparsec (anySingle, eof, runParser)
import Text.Megaparsec.Char (newline)

data Report = Report
  { summary :: !Text,
    shooters :: ![Shooter],
    scores :: ![Score],
    infoMetadata :: ![Text]
  }
  deriving stock (Show)

data ReportFields
  = Title !Text
  | InfoMetadata !Text
  | ZMetadata !Text
  | Summary !Text
  | ShooterHeaderLine ![Text]
  | ShooterLine ![Text]
  | StageHeaderLine ![Text]
  | StageLine ![Text]
  | ScoreHeaderLine ![String]
  | ScoreLine ![String]
  | End
  deriving stock (Show, Eq)

parseReportFields :: String -> Either ParseError ReportFields
parseReportFields fileContent =
  bimap prettifyParseError identity $ runParser (reportFields) mempty fileContent

data MatchInfo = MatchInfo
  { name :: Text,
    date :: Text
  }

toMatchInfo ::
  (MonadUnliftIO m) =>
  ConduitT () ReportFields (ResourceT m) () ->
  ConduitT () Void (ResourceT m) (Maybe MatchInfo)
toMatchInfo reportFieldsStream = do
  reportFieldsStream
    .| Conduit.filterC
      ( \reportFields ->
          case reportFields of
            InfoMetadata metadata ->
              Data.Text.isInfixOf "match name" (Data.Text.toLower metadata)
                || Data.Text.isInfixOf "match date" (Data.Text.toLower metadata)
            _ -> False
      )
    .| accumMatchInfo
    .| decodeMatchInfo
    .| Conduit.headC
  where
    accumMatchInfo :: (Monad m) => ConduitT ReportFields [Text] m ()
    accumMatchInfo = Conduit.concatMapAccumC step Nothing

    step (InfoMetadata metadata) _ = (Just metadata, [])
    step _ currentMetadata = (currentMetadata, [])

    decodeMatchInfo :: (Monad m) => ConduitT [Text] MatchInfo m ()
    decodeMatchInfo =
      evalStateC (MatchInfo {name = "", date = ""}) $
        Conduit.awaitForever $ \metadata -> do
          modify
            ( \state ->
                state
                  { name =
                      fromMaybe "" $
                        find (\metadata -> Data.Text.isInfixOf "match name" $ Data.Text.toLower metadata) metadata,
                    date =
                      fromMaybe "" $
                        find (\metadata -> Data.Text.isInfixOf "match date" $ Data.Text.toLower metadata) metadata
                  }
            )
          metadata <- get
          Conduit.yield metadata

toShooters ::
  (Monad m) =>
  ConduitT () ReportFields m () ->
  ConduitT () Void m [Shooter]
toShooters reportFieldsStream = do
  reportFieldsStream
    .| shooterParser
    .| shooterWithFieldNames
    .| decodeShooter
    .| Conduit.sinkList
  where
    shooterParser :: (Monad m) => ConduitT ReportFields ([Text], [Text]) m ()
    shooterParser = Conduit.concatMapAccumC shooterStepParser Nothing

    shooterStepParser :: ReportFields -> Maybe [Text] -> (Maybe [Text], [([Text], [Text])])
    shooterStepParser (ShooterHeaderLine header) _
      | not (null header) = (Just header, [])
    shooterStepParser (ShooterLine line) currentHeader
      | not (null line),
        Just header <- currentHeader =
          (currentHeader, [(header, line)])
    shooterStepParser _ currentHeader = (currentHeader, [])

toScores ::
  (Monad m) =>
  ConduitT () ReportFields m () ->
  ConduitT () Void m [Score]
toScores reportFieldsStream =
  reportFieldsStream
    .| scoreParser
    .| scoreWithFieldNames
    .| decodeScore
    .| Conduit.sinkList
  where
    scoreParser = Conduit.concatMapAccumC scoreStepParser Nothing

    scoreStepParser :: ReportFields -> Maybe [String] -> (Maybe [String], [([String], [String])])
    scoreStepParser (ScoreHeaderLine header) _
      | not (null header) = (Just header, [])
    scoreStepParser (ScoreLine line) currentHeader
      | not (null line),
        Just header <- currentHeader =
          (currentHeader, [(header, line)])
    scoreStepParser _ currentHeader = (currentHeader, [])

reportFields :: Parser ReportFields
reportFields =
  ((Title <<< toText) <$> title)
    <|> ((InfoMetadata <<< toText) <$> infoLine)
    <|> ((Summary <<< toText) <$> matchSummary)
    <|> ((ZMetadata <<< toText) <$> zMetadata)
    <|> ((ShooterHeaderLine <<< (fmap toText)) <$> shooterHeaderLine)
    <|> ((ShooterLine <<< (fmap toText)) <$> shooterLine)
    <|> ((StageHeaderLine <<< (fmap toText)) <$> stageHeaderLine)
    <|> ((StageLine <<< (fmap toText)) <$> stageLine)
    <|> (ScoreHeaderLine <$> scoreHeader)
    <|> (ScoreLine <$> scoreLine)
    <|> (pure End)

matchSummary :: Parser String
matchSummary = matchIdentifier *> (manyTill anySingle eof)

matchIdentifier :: Parser ()
matchIdentifier = lineStartingWith "A "

zMetadata :: Parser String
zMetadata = zMetadataIdentifier *> (manyTill anySingle eof)

zMetadataIdentifier :: Parser ()
zMetadataIdentifier = lineStartingWith "Z "

info :: Parser [String]
info =
  some
    (infoIdentifier *> (manyTill anySingle newline))

infoLine :: Parser String
infoLine = infoIdentifier *> (manyTill anySingle eof)

infoIdentifier :: Parser ()
infoIdentifier = lineStartingWith "$INFO "

title :: Parser String
title = titleIdentifier *> (manyTill anySingle eof)

titleIdentifier :: Parser ()
titleIdentifier = lineStartingWith "$PRACTISCORE "
