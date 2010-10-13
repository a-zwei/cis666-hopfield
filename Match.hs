module Main where

import Control.Monad
import Data.IORef
import Data.List
import System.Environment

import Hopfield (Hopfield, State)
import qualified Hopfield as H
import Shuffle

--updateAll :: (Hopfield, State) -> IO (Hopfield, State)
--updateAll (h@(H n _ _), xs) = do
--  updateOrder <- shuffle [0..n-1]
--  return $ foldl update (h, xs) updateOrder

pickNeuronF :: Int -> IORef [Int] -> IO Int
pickNeuronF n listRef = do
  list <- readIORef listRef
  if null list then shuffle [0..n-1] >>= writeIORef listRef else return ()
  (i:is) <- readIORef listRef
  writeIORef listRef is
  return i

update :: Int -> IORef (Hopfield, State) -> IO Float
update neuron stateRef = do
  modifyIORef stateRef (H.update neuron)
  liftM H.energy $ readIORef stateRef
--  s <- readIORef stateRef
--  neuron <- pickNeuron
--  let e = energy s
--  let ns = H.update neuron s
--      ne = energy ns
--  writeIORef stateRef ns
--  return ne

loop :: IO Int -> (Hopfield, State) -> [Int] -> IO (Hopfield, State)
loop pickNeuron s@(h, _) noChanges = do
  let e = H.energy s
  putStr "E: "
  print e
  neuron <- pickNeuron
  putStr "Updating neuron "
  print neuron
  let s2 = H.update neuron s
      e2 = H.energy s2
  if e2 < e then loop pickNeuron s2 []
            else do let nnoChanges = noChanges `union` [neuron]
                    if length nnoChanges == H.numNeurons h then return s2
                      else loop pickNeuron s2 nnoChanges

main = do
  [filename, patternFilename] <- getArgs
  h <- H.load filename
  pattern <- liftM read $ readFile patternFilename
  listRef <- newIORef []
  s <- loop (pickNeuronF (H.numNeurons h) listRef) (H.input h pattern) []
  case H.convergence s of Nothing -> putStrLn "Pattern did not match!"
                          Just name -> putStrLn $ "Pattern matched " ++ name
