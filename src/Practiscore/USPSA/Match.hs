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
import Practiscore.USPSA (UspsaMemberId (..))
import Practiscore.USPSA.Parser.Report qualified
import Practiscore.USPSA.Parser.Score qualified
import Practiscore.USPSA.Parser.Shooter qualified
import Practiscore.USPSA.Parser.Stage qualified

type Match = [Stage]

data Stage = Stage
  { shooter :: Practiscore.USPSA.Parser.Shooter.Shooter,
    matchName :: Text,
    matchDate :: Text,
    score :: Practiscore.USPSA.Parser.Score.Score,
    stageNumber :: Word8,
    stageName :: Text,
    classifier :: Bool,
    classifierNumber :: Maybe Text
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
        "match_date",
        "stage_number",
        "stage_name"
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
        "match_date" .= export.matchDate,
        "stage_number" .= export.stageNumber,
        "stage_name" .= export.stageName
      ]

getShooterMatch ::
  UspsaMemberId ->
  Practiscore.USPSA.Parser.Report.MatchInfo ->
  [Practiscore.USPSA.Parser.Stage.StageInfo] ->
  [Practiscore.USPSA.Parser.Shooter.Shooter] ->
  [Practiscore.USPSA.Parser.Score.Score] ->
  Match
getShooterMatch memberId matchInfo stageInfo shooters scores =
  case find (\shooter -> shooter.uspsa == Just memberId) shooters of
    Nothing -> []
    Just shooter ->
      [ Stage
          { shooter,
            score,
            matchName = matchInfo.name,
            matchDate = matchInfo.date,
            classifier =
              case find (\stage -> stage.number == score.stage) stageInfo of
                Nothing -> False
                Just stageInfo -> stageInfo.classifier,
            classifierNumber =
              case find (\stage -> stage.number == score.stage) stageInfo of
                Nothing -> Nothing
                Just stageInfo -> stageInfo.classifierNumber,
            stageNumber =
              case find (\stage -> stage.number == score.stage) stageInfo of
                Nothing -> 0
                Just stageInfo -> stageInfo.number,
            stageName =
              case find (\stage -> stage.number == score.stage) stageInfo of
                Nothing -> ""
                Just stageInfo -> stageInfo.name
          }
        | score <- filter (\score -> score.comp == shooter.comp) scores
      ]

encodeMatch :: [Stage] -> LByteString
encodeMatch match = encodeDefaultOrderedByName match
