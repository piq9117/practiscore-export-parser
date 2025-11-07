module Practiscore.USPSA.Match
  ( Match (..),
    getShooterFromReport,
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
import Practiscore.Parser.Report (Report)
import Practiscore.Parser.Report qualified
import Practiscore.Parser.Score qualified
import Practiscore.Parser.Shooter qualified
import Practiscore.USPSA (CompId (..))

data Match = Match
  { shooter :: Practiscore.Parser.Shooter.Shooter,
    scores :: ![Practiscore.Parser.Score.Score]
  }

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
        "hitFactor",
        "stage_place"
      ]

instance ToNamedRecord Match where
  toNamedRecord export =
    namedRecord
      [ "uspsa_id" .= export.shooter.uspsa,
        "firstname" .= export.shooter.firstname,
        "lastname" .= export.shooter.lastname,
        "class" .= export.shooter.class_,
        "division" .= export.shooter.division,
        "match_points" .= export.shooter.matchPoints,
        "place_overall" .= export.shooter.placeOverall
      ]
      <> ( namedRecord $
             concat
               [ [ "gun" .= score.gun,
                   "stage" .= score.stage,
                   "A" .= score.a,
                   "B" .= score.b,
                   "C" .= score.c,
                   "D" .= score.b,
                   "miss" .= score.miss,
                   "no_shoot" .= score.noShoot,
                   "procedural" .= score.procedural,
                   "time" .= score.time,
                   "raw_points" .= score.rawPoints,
                   "total_points" .= score.totalPoints,
                   "hitFactor" .= score.hitFactor,
                   "stage_place" .= score.stagePlace
                 ]
                 | score <- export.scores
               ]
         )

getShooterFromReport ::
  CompId ->
  Report ->
  Maybe Match
getShooterFromReport compId report = do
  shooter <- find (\shooter -> shooter.comp == Just compId) report.shooters
  pure
    Match
      { shooter,
        scores = filter (\score -> score.comp == Just compId) report.scores
      }

encodeMatch :: Match -> LByteString
encodeMatch match = encodeDefaultOrderedByName [match]
