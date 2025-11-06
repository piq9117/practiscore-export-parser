module Practiscore.Parser.Stage
  ( stageHeaderIdentifier,
    stageHeader,
    stageLines,
    stagesWithFieldName,
    stageLineIdentifier,
  )
where

import Practiscore.Parser (Parser, cells, lineStartingWith)
import Text.Megaparsec.Char (newline)

stageHeader :: Parser [Text]
stageHeader =
  stageHeaderIdentifier *> cells <* newline

stageHeaderIdentifier :: Parser ()
stageHeaderIdentifier = lineStartingWith "F "

stagesWithFieldName :: Parser [[(Text, Text)]]
stagesWithFieldName = do
  header <- stageHeader
  lines <- stageLines
  pure $
    lines <&> \line ->
      zipWith (\h l -> (h, l)) header line

stageLines :: Parser [[Text]]
stageLines = many stageLine

stageLine :: Parser [Text]
stageLine = stageLineIdentifier *> cells <* newline

stageLineIdentifier :: Parser ()
stageLineIdentifier = lineStartingWith "G "
