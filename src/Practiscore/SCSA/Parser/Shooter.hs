{-# LANGUAGE DerivingStrategies #-}

module Practiscore.SCSA.Parser.Shooter
  ( Shooter (..),
    shooterLine,
    decodeShooter,
  )
where

import Conduit (ConduitT, (.|))
import Conduit qualified
import Data.Conduit.Lift (evalStateC)
import Practiscore.Parser (Parser, cells, lineStartingWith)
import Text.Megaparsec (eof)
import Prelude hiding (id)

data Shooter = Shooter
  { id :: Word16,
    memberId :: Text,
    firstname :: Maybe Text,
    lastname :: Maybe Text
  }
  deriving stock (Show, Eq)

shooterHeaders :: [Text]
shooterHeaders =
  [ "id",
    "member id",
    "firstname",
    "lastname"
  ]

decodeShooter :: (Monad m) => ConduitT [Text] Shooter m ()
decodeShooter =
  zipShooterWithHeaders
    .| ( evalStateC (Shooter {id = 0, memberId = mempty, firstname = Nothing, lastname = Nothing}) $
           Conduit.awaitForever
             ( \keyValPairs -> do
                 for_ keyValPairs $ \(header, val) ->
                   case header of
                     "id" ->
                       modify (\shooter -> shooter {id = fromMaybe 0 $ readMaybe $ toString val})
                     "member id" ->
                       modify (\shooter -> shooter {memberId = val})
                     "firstname" ->
                       modify (\shooter -> shooter {firstname = Just val})
                     "lastname" ->
                       modify (\shooter -> shooter {lastname = Just val})
                     _ -> pure ()
                 shooter <- get
                 Conduit.yield shooter
             )
       )

shooterLine :: Parser [String]
shooterLine = lineStartingWith "EC," *> cells <* eof

zipShooterWithHeaders :: (Monad m) => ConduitT [Text] [(Text, Text)] m ()
zipShooterWithHeaders = Conduit.concatMapAccumC step [[]]
  where
    step :: [Text] -> [[(Text, Text)]] -> ([[(Text, Text)]], [[(Text, Text)]])
    step line accum
      | not (null line) = (accum, [zipWith (\h l -> (h, l)) shooterHeaders line])
    step _ accum = (accum, [])
