{-# LANGUAGE DerivingStrategies #-}

module Practiscore.USPSA.Match
  ( Match,
    Stage (..),
    getShooterMatch,
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
import Practiscore.Parser.Report qualified
import Practiscore.Parser.Score qualified
import Practiscore.Parser.Shooter qualified
import Practiscore.USPSA (UspsaMemberId (..))

type Match = [Stage]

data Stage = Stage
  { shooter :: Practiscore.Parser.Shooter.Shooter,
    matchName :: Text,
    matchDate :: Text,
    score :: Practiscore.Parser.Score.Score
  }
  deriving stock (Show)

instance DefaultOrdered Stage where
  headerOrder _ =
    header
      [ "uspsa_id",
        "firstname",
        "lastname",
        "class",
        "division",
        "match_points",
        "place_overall",
        "gun",
        "stage",
        "A",
        "B",
        "C",
        "D",
        "miss",
        "no_shoot",
        "procedural",
        "time",
        "raw_points",
        "total_points",
        "hit_factor",
        "stage_place",
        "match_name",
        "match_date"
      ]

instance ToNamedRecord Stage where
  toNamedRecord export =
    namedRecord
      [ "uspsa_id" .= fmap (.unUspsaMemberId) export.shooter.uspsa,
        "firstname" .= export.shooter.firstname,
        "lastname" .= export.shooter.lastname,
        "class" .= export.shooter.class_,
        "division" .= export.shooter.division,
        "match_points" .= export.shooter.matchPoints,
        "place_overall" .= export.shooter.placeOverall,
        "gun" .= export.score.gun,
        "stage" .= export.score.stage,
        "A" .= export.score.a,
        "B" .= export.score.b,
        "C" .= export.score.c,
        "D" .= export.score.b,
        "miss" .= export.score.miss,
        "no_shoot" .= export.score.noShoot,
        "procedural" .= export.score.procedural,
        "time" .= export.score.time,
        "raw_points" .= export.score.rawPoints,
        "total_points" .= export.score.totalPoints,
        "hit_factor" .= export.score.hitFactor,
        "stage_place" .= export.score.stagePlace,
        "match_name" .= export.matchName,
        "match_date" .= export.matchDate
      ]

getShooterMatch ::
  UspsaMemberId ->
  Practiscore.Parser.Report.MatchInfo ->
  [Practiscore.Parser.Shooter.Shooter] ->
  [Practiscore.Parser.Score.Score] ->
  Match
getShooterMatch memberId matchInfo shooters scores =
  case find (\shooter -> shooter.uspsa == Just memberId) shooters of
    Nothing -> []
    Just shooter ->
      [ Stage
          { shooter,
            score,
            matchName = matchInfo.name,
            matchDate = matchInfo.date
          }
        | score <- filter (\score -> score.comp == shooter.comp) scores
      ]

encodeMatch :: [Stage] -> LByteString
encodeMatch match = encodeDefaultOrderedByName match
