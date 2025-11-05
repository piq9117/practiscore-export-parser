module Spec (main) where

import Spec.Parser.Shooter qualified
import Spec.Parser.Stage qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  (defaultMain <<< testGroup "Parser Spec")
    =<< sequence
      [ Spec.Parser.Shooter.testTree,
        Spec.Parser.Stage.testTree
      ]
