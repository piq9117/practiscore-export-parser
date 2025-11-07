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

type Parser = Parsec Void String

lineStartingWith :: String -> Parser ()
lineStartingWith prefix = try $ do
  _ <- string prefix
  notFollowedBy (char ' ')

cells :: Parser [String]
cells =
  cell `sepBy` ","

cell :: Parser String
cell = many (noneOf @[] ",\n\r")
