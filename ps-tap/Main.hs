module Main (main) where

import Practiscore.CLI (parseCLI)

main :: IO ()
main = do
  cli <- parseCLI
  putStrLn (show cli)
