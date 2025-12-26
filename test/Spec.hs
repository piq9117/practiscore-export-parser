module Spec (main) where

import Spec.SCSA.Parser.Report qualified
import Spec.USPSA.Match qualified
import Spec.USPSA.Parser.Report qualified
import Spec.USPSA.Parser.Score qualified
import Spec.USPSA.Parser.Shooter qualified
import Spec.USPSA.Parser.Stage qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  (defaultMain <<< testGroup "Parser Spec")
    =<< sequence
      [ Spec.USPSA.Parser.Shooter.testTree,
        Spec.USPSA.Parser.Stage.testTree,
        Spec.USPSA.Parser.Score.testTree,
        Spec.USPSA.Parser.Report.testTree,
        Spec.USPSA.Match.testTree,
        Spec.SCSA.Parser.Report.testTree
      ]
