module Spec (main) where

import Spec.Parser.Report qualified
import Spec.Parser.Score qualified
import Spec.Parser.Shooter qualified
import Spec.Parser.Stage qualified
import Spec.USPSA.Match qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  (defaultMain <<< testGroup "Parser Spec")
    =<< sequence
      [ Spec.Parser.Shooter.testTree,
        Spec.Parser.Stage.testTree,
        Spec.Parser.Score.testTree,
        Spec.Parser.Report.testTree,
        Spec.USPSA.Match.testTree
      ]
