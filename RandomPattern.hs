module Main where

import Control.Monad
import System.Environment
import System.Random

convert True = 1
convert False = -1

main = do
  [lenStr, patternFilename] <- getArgs
  list <- replicateM (read lenStr) randomIO
  writeFile patternFilename $ show $ map convert list
