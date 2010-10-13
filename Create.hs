module Main where

import System.Environment

import qualified Hopfield as H

main = do
  [filename, numNeurons] <- getArgs
  H.save filename (H.empty (read numNeurons))
