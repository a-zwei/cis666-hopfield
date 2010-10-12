module Shuffle where

import System.Random
import System.Random.Shuffle (shuffle')

shuffle :: [a] -> IO [a]
shuffle list = do g <- newStdGen
                  return $ shuffle' list (length list) g
