{-# LANGUAGE DerivingStrategies #-}

module Practiscore.USPSA.Match
  ( Match (..),
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
import Practiscore.Parser.Score qualified
import Practiscore.Parser.Shooter qualified
import Practiscore.USPSA (UspsaMemberId (..))

data Match = Match
  { shooter :: Practiscore.Parser.Shooter.Shooter,
    score :: Practiscore.Parser.Score.Score
  }
  deriving stock (Show)

instance DefaultOrdered Match where
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
        "stage_place"
      ]

instance ToNamedRecord Match where
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
        "stage_place" .= export.score.stagePlace
      ]

getShooterMatch :: 
  UspsaMemberId ->
  [Practiscore.Parser.Shooter.Shooter] -> 
  [Practiscore.Parser.Score.Score] ->
  [Match]
getShooterMatch memberId shooters scores =
  case find (\shooter -> shooter.uspsa == Just memberId) shooters of
    Nothing -> []
    Just shooter -> 
      [ Match
          { shooter,
            score
          }
        | score <- filter (\score -> score.comp == shooter.comp) scores
      ]

encodeMatch :: [Match] -> LByteString
encodeMatch match = encodeDefaultOrderedByName match
