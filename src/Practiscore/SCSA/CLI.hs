module Practiscore.SCSA.CLI (streamRawReport) where

import Conduit (ConduitT, MonadResource, MonadThrow, (.|))
import Conduit qualified
import Practiscore.SCSA.Parser.Report (ReportFields, reportFieldStream)

streamRawReport ::
  (MonadThrow m, MonadResource m) =>
  FilePath ->
  ConduitT a ReportFields m ()
streamRawReport filePath =
  Conduit.sourceFile filePath
    .| Conduit.linesUnboundedAsciiC
    .| Conduit.mapC decodeUtf8
    .| reportFieldStream
