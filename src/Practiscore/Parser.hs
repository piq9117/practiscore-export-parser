{-# LANGUAGE DerivingStrategies #-}

module Practiscore.Parser
  ( Parser,
    ParseError (..),
    prettifyParseError,
    lineStartingWith,
    cell,
    cells,
  )
where

import Control.Monad.Combinators (manyTill, sepBy)
import Text.Megaparsec (Parsec, anySingle, noneOf, notFollowedBy, try)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)

type Parser = Parsec Void String

lineStartingWith :: String -> Parser ()
lineStartingWith prefix = try $ do
  _ <- string prefix
  notFollowedBy (char ' ')

cells :: Parser [String]
cells =
  cell `sepBy` ","

cell :: Parser String
cell = quoted <|> unquoted
  where
    quoted = char '"' *> manyTill anySingle (char '"')
    unquoted = many (noneOf @[] ",\n\r")

data ParseError = ParseError Text
  deriving stock (Show)

instance Exception ParseError

prettifyParseError :: ParseErrorBundle String Void -> ParseError
prettifyParseError parseError = ParseError (toText $ errorBundlePretty parseError)
