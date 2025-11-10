module Practiscore.Parser.Stage
  ( stageHeaderIdentifier,
    stageHeader,
    stageLines,
    stagesWithFieldName,
    stageLineIdentifier,
    stageHeaderLine,
    stageLine,
  )
where

import Practiscore.Parser (Parser, cells, lineStartingWith)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (newline)

stageHeader :: Parser [String]
stageHeader =
  stageHeaderIdentifier *> cells <* newline

stageHeaderLine :: Parser [String]
stageHeaderLine =
  stageHeaderIdentifier *> cells <* eof

stageHeaderIdentifier :: Parser ()
stageHeaderIdentifier = lineStartingWith "F "

stagesWithFieldName :: Parser [[(String, String)]]
stagesWithFieldName = do
  header <- stageHeader
  lines <- stageLines
  pure $
    lines <&> \line ->
      zipWith (\h l -> (h, l)) header line

stageLines :: Parser [[String]]
stageLines = many stageLine

stageLine :: Parser [String]
stageLine = stageLineIdentifier *> cells <* eof

stageLineIdentifier :: Parser ()
stageLineIdentifier = lineStartingWith "G "
