{-# LANGUAGE DerivingStrategies #-}

module Practiscore.CLI (CLI (..), parseCLI) where

import Control.Exception (throwIO)
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
import Practiscore.Parser.Shooter (toUspsaMemberId)
import Practiscore.USPSA.CLI qualified
import Practiscore.USPSA.Match (encodeMatch, getShooterFromReport)

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
  deriving stock (Show)

instance Exception CliParseErrors

parseCLI :: IO ()
parseCLI = do
  cli <- showHelpOnErrorOnExecParser (info (helper <*> cli) fullDesc)
  report <- Practiscore.USPSA.CLI.readReport cli.reportPath
  case toUspsaMemberId cli.uspsaMemberId of
    Nothing -> throwIO (InvalidUspsaMemberId $ cli.uspsaMemberId <> " is not valid id")
    Just uspsaMemberId ->
      writeFileBS cli.output (toStrict $ encodeMatch $ getShooterFromReport uspsaMemberId report)

showHelpOnErrorOnExecParser :: ParserInfo a -> IO a
showHelpOnErrorOnExecParser = customExecParser (prefs showHelpOnError)
