module Practiscore.Parser
  ( Parser,
    lineStartingWith,
    cell,
    cells,
  )
where

import Control.Monad.Combinators (sepBy)
import Text.Megaparsec (Parsec, noneOf, notFollowedBy, try)
import Text.Megaparsec.Char (char, string)

type Parser = Parsec Void Text

lineStartingWith :: Text -> Parser ()
lineStartingWith prefix = try $ do
  _ <- string prefix
  notFollowedBy (char ' ')

cells :: Parser [Text]
cells =
  cell `sepBy` ","

cell :: Parser Text
cell = fmap toText $ many (noneOf @[] ",\n\r")
