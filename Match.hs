module Main where

import Control.Monad
import Data.IORef
import Data.List
import System.Environment

import Hopfield (NetState)
import qualified Hopfield as H
import Shuffle

-- 'pickNeuronF n r' returns a random number in [0..n-1] by shuffling
pickNeuronF :: Int -> IORef [Int] -> IO Int
pickNeuronF n listRef = do
  list <- readIORef listRef
  if null list then shuffle [0..n-1] >>= writeIORef listRef else return ()
  (i:is) <- readIORef listRef
  writeIORef listRef is
  return i

loop :: IO Int -> NetState -> [Int] -> Int -> IO (NetState, Int)
loop pickNeuron s@(h, _) notChanged count = do
  let e = H.energy s
  putStrLn $ "E: " ++ show e
  neuron <- pickNeuron
  putStrLn $ "Updating neuron " ++ show neuron
  let s2 = H.update neuron s
      e2 = H.energy s2
  if e2 < e then loop pickNeuron s2 [] (count + 1)
            else do let nnotChanged = notChanged `union` [neuron]
                    if length nnotChanged == H.numNeurons h
                      then return (s2, count)
                      else loop pickNeuron s2 nnotChanged (count + 1)

main = do
  [filename, patternFilename] <- getArgs
  h <- H.load filename
  pattern <- liftM read $ readFile patternFilename
  listRef <- newIORef []
  (s, c) <- loop (pickNeuronF (H.numNeurons h) listRef) (H.input h pattern) [] 1
  putStr $ "After " ++ show c ++ " iterations, "
  case H.convergence s of Nothing -> putStrLn "pattern did not match!"
                          Just name -> putStrLn $ "pattern matched " ++ name
