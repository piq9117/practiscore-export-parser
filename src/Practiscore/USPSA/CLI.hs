{-# LANGUAGE DerivingStrategies #-}

module Practiscore.USPSA.CLI (streamRawReport) where

import Conduit (ConduitT, MonadResource, MonadThrow, (.|))
import Conduit qualified
import Practiscore.USPSA.Parser.Report (ReportFields, parseReportFields)

data ParseError = ParseError Text
  deriving stock (Show)

instance Exception ParseError

streamRawReport ::
  (MonadThrow m, MonadResource m) =>
  FilePath ->
  ConduitT a ReportFields m ()
streamRawReport filePath =
  Conduit.sourceFile filePath
    .| Conduit.linesUnboundedAsciiC
    .| Conduit.mapMC
      ( \rawReport ->
          case parseReportFields $ decodeUtf8 rawReport of
            Left err -> Conduit.throwM err
            Right reportField -> pure reportField
      )
