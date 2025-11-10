{-# LANGUAGE DerivingStrategies #-}

module Practiscore.CLI (CLI (..), parseCLI) where

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
  _cli <- showHelpOnErrorOnExecParser (info (helper <*> cli) fullDesc)
  pure ()

showHelpOnErrorOnExecParser :: ParserInfo a -> IO a
showHelpOnErrorOnExecParser = customExecParser (prefs showHelpOnError)
