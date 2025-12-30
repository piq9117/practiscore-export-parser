{-# LANGUAGE DerivingStrategies #-}

module Practiscore.SCSA.Parser.Stage
  ( Stage (..),
    stageLine,
    decodeStage,
  )
where

import Conduit (ConduitT, (.|))
import Conduit qualified
import Data.Conduit.Lift (evalStateC)
import Data.Text qualified
import Practiscore.Parser (Parser, cells, lineStartingWith)
import Text.Megaparsec (eof)
import Prelude hiding (id)

data Stage = Stage
  { id :: Word16,
    name :: Text,
    classifierCode :: Maybe Text
  }
  deriving stock (Show, Eq)

stageLine :: Parser [String]
stageLine = lineStartingWith "ST," *> cells <* eof

-- | stage headers are not part of the file export from practiscore.
-- So this is an assumption based on what is in the file output.
--
-- Example stage line
-- ```
-- ST,1,"Speed Option",SC-107,1,1,5,TRUE,FALSE,0.00,3.00,,,,,,,,,,,,,,,
-- ```
stageHeaders :: [Text]
stageHeaders = ["id", "name", "classifier code"]

decodeStage :: (Monad m) => ConduitT [Text] Stage m ()
decodeStage =
  zipStageWithHeaders
    .| ( evalStateC (Stage {id = 0, name = mempty, classifierCode = mempty}) $
           Conduit.awaitForever $
             \keyValPairs -> do
               for_ keyValPairs $ \(header, val) ->
                 case header of
                   "id" ->
                     modify (\stage -> stage {id = fromMaybe 0 $ readMaybe $ toString val})
                   "name" ->
                     modify (\stage -> stage {name = Data.Text.filter (/= '"') val})
                   "classifier code" ->
                     modify
                       ( \stage ->
                           stage
                             { classifierCode =
                                 if Data.Text.strip val == mempty
                                   then
                                     Nothing
                                   else Just val
                             }
                       )
                   _ -> pure ()
               stage <- get
               Conduit.yield stage
       )

zipStageWithHeaders :: (Monad m) => ConduitT [Text] [(Text, Text)] m ()
zipStageWithHeaders = Conduit.concatMapAccumC step [[]]
  where
    step :: [Text] -> [[(Text, Text)]] -> ([[(Text, Text)]], [[(Text, Text)]])
    step line accum
      | not (null line) = (accum, [zipWith (\h l -> (h, l)) stageHeaders line])
    step _ accum = (accum, [])
