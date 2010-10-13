module Main where

import Control.Monad
import System.Environment
import System.Random

-- 'flipProb p v' has p probability of negating v
flipProb :: Float -> Int -> IO Int
flipProb p v = do
  r <- randomRIO (0, 1)
  if r < p then return (-v)
           else return v

main = do
  [patternFilename, newPatternFilename, percentStr] <- getArgs
  patternStr <- readFile patternFilename
  npattern <- forM (read patternStr) $ flipProb ((read percentStr) / 100)
  writeFile newPatternFilename $ show npattern
