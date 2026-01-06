{-# LANGUAGE DerivingStrategies #-}

module Practiscore.SCSA.Parser.Division
  ( Division (..),
    decodeDivision,
    divisionLine,
    emptyDivision,
  )
where

import Conduit (ConduitT, (.|))
import Conduit qualified
import Data.Conduit.Lift (evalStateC)
import Practiscore.Parser (Parser, cells, lineStartingWith)
import Text.Megaparsec (eof)

-- | Every division is assigned to a different shooter id
data Division = Division
  { recordType :: Text,
    order :: Text,
    shooterId :: Word16,
    matchTypeId :: Text,
    divisionCode :: Text
  }
  deriving stock (Eq, Show)

decodeDivision :: (Monad m) => ConduitT [Text] Division m ()
decodeDivision =
  zipDivisionWithHeaders
    .| ( evalStateC emptyDivision $ Conduit.awaitForever $ \keyValPairs -> do
           for_ keyValPairs $ \(header, val) ->
             case header of
               "record type" -> modify (\division -> division {recordType = val})
               "order" -> modify (\division -> division {order = val})
               "shooter id" -> modify (\division -> division {shooterId = fromMaybe 0 $ readMaybe $ toString val})
               "match type id" -> modify (\division -> division {matchTypeId = val})
               "division code" -> modify (\division -> division {divisionCode = val})
               _ -> pure ()
           division <- get
           Conduit.yield division
       )

emptyDivision :: Division
emptyDivision =
  Division
    { recordType = mempty,
      order = mempty,
      shooterId = 0,
      matchTypeId = mempty,
      divisionCode = mempty
    }

divisionHeaders :: [Text]
divisionHeaders =
  [ "record type",
    "order",
    "shooter id",
    "match type id",
    "division code"
  ]

divisionLine :: Parser [String]
divisionLine = lineStartingWith "CO," *> cells <* eof

zipDivisionWithHeaders :: (Monad m) => ConduitT [Text] [(Text, Text)] m ()
zipDivisionWithHeaders = Conduit.concatMapAccumC step [[]]
  where
    step :: [Text] -> [[(Text, Text)]] -> ([[(Text, Text)]], [[(Text, Text)]])
    step line accum
      | not (null line) = (accum, [zipWith (\h l -> (h, l)) divisionHeaders line])
    step _ accum = (accum, [])
