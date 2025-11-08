{-# LANGUAGE DerivingStrategies #-}

module Practiscore.Parser.Report
  ( Report (..),
    ReportFields (..),
    reportFields,
    parseReport,
    title,
    info,
    zMetadata,
    matchSummary,
    reportFieldsToReport,
    parseReportFields
  )
where

import Control.Applicative.Combinators (manyTill)
import Practiscore.Parser (Parser, lineStartingWith)
import Practiscore.Parser.Score (Score, decodeScores)
import Practiscore.Parser.Shooter (Shooter, decodeShooters)
import Practiscore.Parser.Stage (stagesWithFieldName)
import Text.Megaparsec (anySingle, runParser)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Error (ParseErrorBundle)

data Report = Report
  { summary :: !Text,
    shooters :: ![Shooter],
    scores :: ![Score],
    infoMetadata :: ![Text]
  }
  deriving stock (Show)

emptyReport :: Report
emptyReport = Report 
  { summary = "",
    shooters = [],
    scores = [],
    infoMetadata = []
  }

reportFieldsToReport :: [ReportFields] -> Report
reportFieldsToReport reportFields = 
  foldr (\fields accum -> 
    case fields of
      Summary summary -> accum { summary = summary }
      Shooters shooters -> accum { shooters = shooters }
      Scores scores -> accum { scores = scores }
      InfoMetadata infoMetadata -> accum { infoMetadata = infoMetadata }
      _ -> accum
    ) emptyReport reportFields

data ReportFields
  = Title !Text
  | Summary !Text
  | Shooters ![Shooter]
  | Scores ![Score]
  | Stages ![[(String, String)]]
  | ZMetadata !Text
  | InfoMetadata ![Text]
  deriving stock (Show, Eq)

parseReport :: String -> Either (ParseErrorBundle String Void) Report
parseReport fileContent =
  runParser
    report
    mempty
    fileContent

parseReportFields :: String -> Either (ParseErrorBundle String Void) [ReportFields]
parseReportFields fileContent =
  runParser (many reportFields) mempty fileContent

reportFields :: Parser ReportFields
reportFields =
  ((Title <<< toText) <$> title)
    <|> ((InfoMetadata <<< (fmap toText)) <$> info)
    <|> (Summary <$> matchSummary)
    <|> ((ZMetadata <<< toText) <$> zMetadata)
    <|> (Shooters <$> decodeShooters)
    <|> (Stages <$> stagesWithFieldName)
    <|> (Scores <$> decodeScores)

report :: Parser Report
report = do
  _ <- title
  info <- info
  _ <- zMetadata
  summary <- matchSummary
  shooters <- decodeShooters
  _ <- stagesWithFieldName
  scores <- decodeScores
  pure
    Report
      { summary,
        shooters,
        scores,
        infoMetadata = fmap toText info
      }

matchSummary :: Parser Text
matchSummary = matchIdentifier *> (fmap toText $ manyTill anySingle newline)

matchIdentifier :: Parser ()
matchIdentifier = lineStartingWith "A "

zMetadata :: Parser String
zMetadata = zMetadataIdentifier *> (manyTill anySingle newline)

zMetadataIdentifier :: Parser ()
zMetadataIdentifier = lineStartingWith "Z "

info :: Parser [String]
info =
  some
    (infoIdentifier *> (manyTill anySingle newline))

infoIdentifier :: Parser ()
infoIdentifier = lineStartingWith "$INFO "

title :: Parser String
title = titleIdentifier *> (manyTill anySingle newline)

titleIdentifier :: Parser ()
titleIdentifier = lineStartingWith "$PRACTISCORE "
