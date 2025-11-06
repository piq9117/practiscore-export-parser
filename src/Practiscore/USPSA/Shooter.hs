module Practiscore.USPSA.Shooter
  ( Shooter (..),
    getShooterFromReport,
  )
where

import Practiscore.Parser.Report (Report)
import Practiscore.Parser.Report qualified
import Practiscore.Parser.Score qualified
import Practiscore.Parser.Shooter qualified
import Practiscore.USPSA (CompId)

data Shooter = Shooter
  { matchInfo :: Practiscore.Parser.Shooter.Shooter,
    scores :: ![Practiscore.Parser.Score.Score]
  }

getShooterFromReport ::
  CompId ->
  Report ->
  Maybe Shooter
getShooterFromReport compId report = do
  shooter <- find (\shooter -> shooter.comp == Just compId) report.shooters
  pure
    Shooter
      { matchInfo = shooter,
        scores = filter (\score -> score.comp == Just compId) report.scores
      }
