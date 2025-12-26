{-# LANGUAGE DerivingStrategies #-}

module Practiscore.SCSA.Parser.Report
  ( ReportFields (..),
    reportFields,
  )
where

import Control.Applicative.Combinators (manyTill)
import Practiscore.Parser (Parser, lineStartingWith)
import Text.Megaparsec (anySingle, eof)

data ReportFields
  = InfoMetadata !Text
  deriving stock (Show, Eq)

reportFields :: Parser ReportFields
reportFields =
  (InfoMetadata <<< toText) <$> infoLine

infoLine :: Parser String
infoLine = lineStartingWith "ER," *> (manyTill anySingle eof)
