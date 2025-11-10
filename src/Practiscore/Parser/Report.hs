{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Practiscore.Parser.Report
  ( Report (..),
    ReportFields (..),
    reportFields,
    title,
    info,
    infoLine,
    zMetadata,
    matchSummary,
    parseReportFields,
    toShooters,
  )
where

import Conduit (ConduitT, MonadUnliftIO, ResourceT, (.|))
import Conduit qualified
import Control.Applicative.Combinators (manyTill)
import Practiscore.Parser (Parser, lineStartingWith)
import Practiscore.Parser.Score (Score, scoreHeader, scoreLine)
import Practiscore.Parser.Shooter
  ( Shooter (..),
    shooterHeaderLine,
    shooterLine,
    shooterWithFieldNames,
    decodeShooter
  )
import Practiscore.Parser.Stage (stageHeaderLine, stageLine)
import Text.Megaparsec (anySingle, eof, runParser)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Error (ParseErrorBundle)

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
  | ScoreHeaderLine ![Text]
  | ScoreLine ![Text]
  | End
  deriving stock (Show, Eq)

parseReportFields :: String -> Either (ParseErrorBundle String Void) ReportFields
parseReportFields fileContent =
  runParser (reportFields) mempty fileContent

toShooters ::
  (MonadUnliftIO m) =>
  ConduitT () ReportFields (ResourceT m) () ->
  m [Shooter]
toShooters reportFieldsStream = do
  Conduit.runConduitRes $
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
    <|> ((ScoreHeaderLine <<< (fmap toText)) <$> scoreHeader)
    <|> ((ScoreLine <<< (fmap toText)) <$> scoreLine)
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
