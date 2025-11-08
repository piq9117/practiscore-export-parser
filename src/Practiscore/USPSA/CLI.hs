{-# LANGUAGE DerivingStrategies #-}

module Practiscore.USPSA.CLI where

import Control.Exception (throwIO)
import Practiscore.Parser.Report (Report, parseReport)

data ParseError = ParseError Text
  deriving stock (Show)

instance Exception ParseError

readReport :: FilePath -> IO Report
readReport filePath = do
  fileContent <- readFileBS filePath
  case parseReport (decodeUtf8 fileContent) of
    Left _err -> throwIO (ParseError $ "error parsing file: " <> show filePath)
    Right report -> pure report
