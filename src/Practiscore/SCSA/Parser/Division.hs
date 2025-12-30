module Practiscore.SCSA.Parser.Division (divisionLine) where

import Practiscore.Parser (Parser, cells, lineStartingWith)
import Text.Megaparsec (eof)

divisionLine :: Parser [String]
divisionLine = lineStartingWith "CO," *> cells <* eof
