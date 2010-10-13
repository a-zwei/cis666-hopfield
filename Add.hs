module Main where

import System.Environment

import qualified Hopfield as H

main = do
  [filename, newFilename, patternFilename] <- getArgs
  h <- H.load filename
  patternStr <- readFile patternFilename
  H.save newFilename $ H.add h (read patternStr, patternFilename)
