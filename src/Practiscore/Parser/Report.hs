{-# LANGUAGE DerivingStrategies #-}

module Practiscore.Parser.Report
  ( Report (..),
    parseReport,
    title,
    info,
    zMetadata,
    matchSummary,
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

parseReport :: String -> Either (ParseErrorBundle String Void) Report
parseReport fileContent =
  runParser
    report
    mempty
    fileContent

report :: Parser Report
report = do
  _ <- title
  info <- info
  _ <- zMetadata
  summary <- matchSummary
  shooters <- decodeShooters
  _ <- stagesWithFieldName
  scores <- decodeScores
  -- end
  -- pure () <|> eof <|> (eol *> pure ())
  pure
    Report
      { summary,
        shooters,
        scores,
        infoMetadata = info
      }

matchSummary :: Parser Text
matchSummary = matchIdentifier *> (fmap toText $ manyTill anySingle newline)

matchIdentifier :: Parser ()
matchIdentifier = lineStartingWith "A "

zMetadata :: Parser Text
zMetadata = zMetadataIdentifier *> (fmap toText $ manyTill anySingle newline)

zMetadataIdentifier :: Parser ()
zMetadataIdentifier = lineStartingWith "Z "

info :: Parser [Text]
info =
  many
    ( infoIdentifier
        *> (fmap toText $ manyTill anySingle newline)
    )

infoIdentifier :: Parser ()
infoIdentifier = lineStartingWith "$INFO "

title :: Parser Text
title = titleIdentifier *> (fmap toText $ manyTill anySingle newline)

titleIdentifier :: Parser ()
titleIdentifier = lineStartingWith "$PRACTISCORE "
