module Main where

import Control.Monad
import System.Environment
import System.Random

flipProb :: Float -> Int -> IO Int
flipProb p v = do
  r <- randomRIO (0, 1)
  if r < p then return (-v)
           else return v

main = do
  [patternFilename, newPatternFilename, percentStr] <- getArgs
  patternStr <- readFile patternFilename
  let pattern = read patternStr
      percent = read percentStr
  npattern <- forM pattern $ flipProb (percent / 100)
  writeFile newPatternFilename $ show npattern
