{-# LANGUAGE DerivingStrategies #-}

module Practiscore.CLI (CLI (..), parseCLI) where

import Conduit (MonadThrow, MonadUnliftIO, (.|))
import Conduit qualified
import Control.Monad.Catch (throwM)
import Options.Applicative
  ( Parser,
    ParserInfo,
    customExecParser,
    flag',
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
import Practiscore.SCSA.CLI qualified
import Practiscore.SCSA.Match qualified
import Practiscore.SCSA.Parser.Report qualified
import Practiscore.USPSA.CLI qualified
import Practiscore.USPSA.Match qualified
import Practiscore.USPSA.Parser.Report qualified
import Practiscore.USPSA.Parser.Shooter (toUspsaMemberId)

data SportType
  = USPSA
  | SCSA
  deriving stock (Show, Eq)

uspsaParser :: Parser SportType
uspsaParser = flag' USPSA (long "uspsa" <> help "Indicate USPSA")

scsaParser :: Parser SportType
scsaParser = flag' SCSA (long "scsa" <> help "Indicate SCSA")

data CLI = CLI
  { memberId :: Text,
    sportType :: SportType,
    reportPath :: FilePath,
    output :: FilePath
  }
  deriving stock (Eq, Show)

cli :: Parser CLI
cli =
  CLI
    <$> strOption
      ( long "member-id"
          <> metavar "MEMBER-ID"
          <> help "MEMBER ID"
      )
    <*> (uspsaParser <|> scsaParser)
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
  = InvalidMemberId Text
  | MatchNotFound Text
  | MatchInfoNotFound
  deriving stock (Show)

instance Exception CliParseErrors

parseCLI :: (MonadUnliftIO m, MonadThrow m) => m ()
parseCLI = do
  cli <- liftIO $ showHelpOnErrorOnExecParser (info (helper <*> cli) fullDesc)

  Conduit.runConduitRes $ do
    case cli.sportType of
      USPSA -> do
        let stream = Practiscore.USPSA.CLI.streamRawReport cli.reportPath
        stageInfo <- Practiscore.USPSA.Parser.Report.toStagesInfo stream
        matchInfo <- Practiscore.USPSA.Parser.Report.toMatchInfo stream
        shooters <- Practiscore.USPSA.Parser.Report.toShooters stream
        scores <- Practiscore.USPSA.Parser.Report.toScores stream
        matchInfo <-
          whenNothing matchInfo $
            liftIO $
              throwM MatchInfoNotFound
        case toUspsaMemberId cli.memberId of
          Nothing -> throwM (InvalidMemberId $ cli.memberId <> " is not valid")
          Just memberId ->
            Conduit.yield
              ( toStrict $
                  Practiscore.USPSA.Match.encodeMatch $
                    Practiscore.USPSA.Match.getShooterMatch memberId matchInfo stageInfo shooters scores
              )
              .| Conduit.sinkFile cli.output
      SCSA -> do
        let reportFieldStream = Practiscore.SCSA.CLI.streamRawReport cli.reportPath

        matchInfo <-
          reportFieldStream
            .| Practiscore.SCSA.Parser.Report.toMatchInfoStream
            .| Conduit.headC

        matchInfo <-
          whenNothing matchInfo $
            liftIO $
              throwM MatchInfoNotFound

        stages <-
          reportFieldStream
            .| Practiscore.SCSA.Parser.Report.toStageStream
            .| Conduit.sinkList

        shooters <-
          reportFieldStream
            .| Practiscore.SCSA.Parser.Report.toShooterStream
            .| Conduit.sinkList

        scores <-
          reportFieldStream
            .| Practiscore.SCSA.Parser.Report.toScoreStream
            .| Conduit.sinkList

        divisions <-
          reportFieldStream
            .| Practiscore.SCSA.Parser.Report.toDivisionStream
            .| Conduit.sinkList

        Conduit.yield
          ( toStrict $ Practiscore.SCSA.Match.encodeMatch $
              Practiscore.SCSA.Match.getMatch cli.memberId matchInfo stages scores shooters divisions
          )
          .| Conduit.sinkFile cli.output

showHelpOnErrorOnExecParser :: ParserInfo a -> IO a
showHelpOnErrorOnExecParser = customExecParser (prefs showHelpOnError)
