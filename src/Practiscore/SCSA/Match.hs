{-# LANGUAGE DerivingStrategies #-}

module Practiscore.SCSA.Match
  ( Match,
    MatchRow (..),
    getMatch,
    encodeMatch,
  )
where

import Data.Csv
  ( DefaultOrdered (..),
    ToNamedRecord (..),
    encodeDefaultOrderedByName,
    header,
    namedRecord,
    (.=),
  )
import Practiscore.SCSA.Parser.Division qualified
import Practiscore.SCSA.Parser.Report qualified
import Practiscore.SCSA.Parser.Score qualified
import Practiscore.SCSA.Parser.Shooter qualified
import Practiscore.SCSA.Parser.Stage qualified

type Match = [MatchRow]

data MatchRow = MatchRow
  { shooter :: Practiscore.SCSA.Parser.Shooter.Shooter,
    matchName :: Text,
    matchDate :: Text,
    score :: Practiscore.SCSA.Parser.Score.Score,
    stageId :: Word16,
    stageName :: Text,
    classifierCode :: Maybe Text,
    division :: Practiscore.SCSA.Parser.Division.Division
  }
  deriving stock (Show)

instance DefaultOrdered MatchRow where
  headerOrder _ =
    header
      [ "scsa_id",
        "firstname",
        "lastname",
        "match_name",
        "match_date",
        "stage_id",
        "stage_name",
        "stage_total_time",
        -- string 1
        "string_1_time",
        "string_1_penalty",
        "string_1_dnf",
        -- string 2
        "string_2_time",
        "string_2_penalty",
        "string_2_dnf",
        -- string 3
        "string_3_time",
        "string_3_penalty",
        "string_3_dnf",
        -- string 4
        "string_4_time",
        "string_4_penalty",
        "string_4_dnf",
        -- string 5
        "string_5_time",
        "string_5_penalty",
        "string_5_dnf",
        "classifier_code",
        "division"
      ]

instance ToNamedRecord MatchRow where
  toNamedRecord row =
    namedRecord
      [ "scsa_id" .= row.shooter.memberId,
        "firstname" .= row.shooter.firstname,
        "lastname" .= row.shooter.lastname,
        "match_name" .= row.matchName,
        "match_date" .= row.matchDate,
        "stage_id" .= row.stageId,
        "stage_name" .= row.stageName,
        "stage_total_time" .= row.score.stageTotalTime,
        -- string 1
        "string_1_time" .= row.score.string1Time,
        "string_1_penalty" .= row.score.string1Penalty,
        "string_1_dnf" .= (show @Text row.score.string1DNF),
        -- string 2
        "string_2_time" .= row.score.string2Time,
        "string_2_penalty" .= row.score.string2Penalty,
        "string_2_dnf" .= (show @Text row.score.string2DNF),
        -- string 3
        "string_3_time" .= row.score.string3Time,
        "string_3_penalty" .= row.score.string3Penalty,
        "string_3_dnf" .= (show @Text row.score.string3DNF),
        -- string 4
        "string_4_time" .= row.score.string4Time,
        "string_4_penalty" .= row.score.string4Penalty,
        "string_4_dnf" .= (show @Text row.score.string4DNF),
        -- string 5
        "string_5_time" .= row.score.string5Time,
        "string_5_penalty" .= row.score.string5Penalty,
        "string_5_dnf" .= (show @Text row.score.string5DNF),
        "classifier_code" .= row.classifierCode,
        "division" .= row.division.divisionCode
      ]

encodeMatch :: Match -> LByteString
encodeMatch match = encodeDefaultOrderedByName match

getMatch ::
  -- member id
  Text ->
  Practiscore.SCSA.Parser.Report.MatchInfo ->
  [Practiscore.SCSA.Parser.Stage.Stage] ->
  [Practiscore.SCSA.Parser.Score.Score] ->
  [Practiscore.SCSA.Parser.Shooter.Shooter] ->
  [Practiscore.SCSA.Parser.Division.Division] ->
  Match
getMatch memberId matchInfo stages scores shooters divisions =
  -- In steel challenge if a shooter has more than one gun, they are added in as
  -- different shooter IDs.
  foldr (\v accum -> v <> accum) [] $
    [ [ MatchRow
          { shooter,
            matchName = matchInfo.matchName,
            matchDate = matchInfo.matchDate,
            score,
            stageId = score.stageId,
            stageName = maybe mempty (\stage -> stage.name) $ find (\stage -> stage.id == score.stageId) stages,
            classifierCode = maybe mempty (\stage -> stage.classifierCode) $ find (\stage -> stage.id == score.stageId) stages,
            division
          }
      ]
      | shooter <- filter (\shooter -> shooter.memberId == memberId) shooters,
        score <- filter (\score -> score.shooterId == shooter.id) scores,
        division <- filter (\division -> division.shooterId == shooter.id) divisions
    ]
