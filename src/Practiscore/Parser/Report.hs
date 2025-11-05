{-# LANGUAGE DerivingStrategies #-}

module Practiscore.Parser.Report where

import Control.Applicative.Combinators (manyTill)
import Practiscore.Parser (Parser, lineStartingWith)
import Practiscore.Parser.Shooter (Shooter)
import Practiscore.Parser.Shooter qualified
import Practiscore.Parser.Stage (stageHeader)
import Text.Megaparsec (anySingle, runParser)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Error (ParseErrorBundle)

data Report = Report
  { summary :: Text,
    shooters :: ![Shooter],
    infoMetadata :: [Text]
  }
  deriving stock (Show)

parseReport :: Text -> Either (ParseErrorBundle Text Void) Report
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
  shooters <- Practiscore.Parser.Shooter.decodeShooters
  _ <- stageHeader
  pure ()
  pure
    Report
      { summary,
        shooters,
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
info = many $ do
  infoIdentifier
  fmap toText $ manyTill anySingle newline

infoIdentifier :: Parser ()
infoIdentifier = lineStartingWith "$INFO "

title :: Parser Text
title = titleIdentifier *> (fmap toText $ manyTill anySingle newline)

titleIdentifier :: Parser ()
titleIdentifier = lineStartingWith "$PRACTISCORE "
