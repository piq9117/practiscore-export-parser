{-# LANGUAGE DerivingStrategies #-}

module Practiscore.SCSA.Parser.Report
  ( ReportFields (..),
    reportFields,
    reportFieldStream,
    toStageStream,
    toShooterStream,
    toScoreStream,
  )
where

import Conduit (ConduitT, MonadThrow, (.|))
import Conduit qualified
import Control.Applicative.Combinators (manyTill)
import Practiscore.Parser (ParseError, Parser, lineStartingWith, prettifyParseError)
import Practiscore.SCSA.Parser.Score (Score (..), decodeScore, scoreLine)
import Practiscore.SCSA.Parser.Shooter (Shooter (..), decodeShooter, shooterLine)
import Practiscore.SCSA.Parser.Stage (Stage (..), decodeStage, stageLine)
import Text.Megaparsec (anySingle, eof, runParser)

data ReportFields
  = InfoMetadata !Text
  | ShooterLine ![Text]
  | StageLine ![Text]
  | ScoreLine ![Text]
  deriving stock (Show, Eq)

parseReportFields :: String -> Either ParseError ReportFields
parseReportFields fileContent =
  bimap prettifyParseError identity $ runParser reportFields mempty fileContent

reportFields :: Parser ReportFields
reportFields =
  ((InfoMetadata <<< toText) <$> infoLine)
    <|> ((ShooterLine <<< (fmap toText)) <$> shooterLine)
    <|> ((StageLine <<< (fmap toText)) <$> stageLine)
    <|> ((ScoreLine <<< (fmap toText)) <$> scoreLine)

infoLine :: Parser String
infoLine = lineStartingWith "ER," *> (manyTill anySingle eof)

reportFieldStream :: (MonadThrow m, Monad m) => ConduitT String ReportFields m ()
reportFieldStream =
  Conduit.awaitForever
    ( \fileContent ->
        case parseReportFields fileContent of
          Left err -> Conduit.throwM err
          Right reportField -> Conduit.yield reportField
    )

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
