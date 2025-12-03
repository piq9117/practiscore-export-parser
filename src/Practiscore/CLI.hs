{-# LANGUAGE DerivingStrategies #-}

module Practiscore.CLI (CLI (..), parseCLI) where

import Conduit (MonadThrow, MonadUnliftIO, (.|))
import Conduit qualified
import Control.Monad.Catch (throwM)
import Options.Applicative
  ( Parser,
    ParserInfo,
    customExecParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    prefs,
    showHelpOnError,
    strOption,
  )
import Practiscore.Parser.Report qualified
import Practiscore.Parser.Shooter (toUspsaMemberId)
import Practiscore.USPSA.CLI (streamRawReport)
import Practiscore.USPSA.Match (encodeMatch, getShooterMatch)

data CLI = CLI
  { uspsaMemberId :: Text,
    reportPath :: FilePath,
    output :: FilePath
  }
  deriving stock (Eq, Show)

cli :: Parser CLI
cli =
  CLI
    <$> strOption
      ( long "uspsa-id"
          <> metavar "USPSA-ID"
          <> help "USPSA MEMBER ID"
      )
    <*> strOption
      ( long "report"
          <> metavar "REPORT_PATH"
          <> help "Relative path of the report"
      )
    <*> strOption
      ( long "output"
          <> metavar "FILE TO OUTPUT INDIVIDUAL SHOOTER DATA"
          <> help "Relative path to output individual report"
      )

data CliParseErrors
  = InvalidUspsaMemberId Text
  | MatchNotFound Text
  | MatchInfoNotFound
  deriving stock (Show)

instance Exception CliParseErrors

parseCLI :: (MonadUnliftIO m, MonadThrow m) => m ()
parseCLI = do
  cli <- liftIO $ showHelpOnErrorOnExecParser (info (helper <*> cli) fullDesc)

  Conduit.runConduitRes $ do
    let stream = streamRawReport cli.reportPath
    matchInfo <- Practiscore.Parser.Report.toMatchInfo stream
    shooters <- Practiscore.Parser.Report.toShooters stream
    scores <- Practiscore.Parser.Report.toScores stream
    matchInfo <-
      whenNothing matchInfo $
        liftIO $
          throwM MatchInfoNotFound
    case toUspsaMemberId cli.uspsaMemberId of
      Nothing -> throwM (InvalidUspsaMemberId $ cli.uspsaMemberId <> " is not valid")
      Just uspsaMemberId ->
        Conduit.yield (toStrict $ encodeMatch $ getShooterMatch uspsaMemberId matchInfo shooters scores)
          .| Conduit.sinkFile cli.output

showHelpOnErrorOnExecParser :: ParserInfo a -> IO a
showHelpOnErrorOnExecParser = customExecParser (prefs showHelpOnError)
