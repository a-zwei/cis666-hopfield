module Hopfield where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Shuffle

type Pattern = (String, [Int])

data Hopfield = H Int (Map (Int, Int) Int) [Pattern]
  deriving (Eq, Read, Show)

type State = Seq Int

si = Seq.index

weight :: Hopfield -> Int -> Int -> Int
weight (H _ ws _) i j | i == j = 0
                      | i < j = ws Map.! (i, j)
                      | i > j = ws Map.! (j, i)

add :: Hopfield -> Pattern -> Hopfield
add (H n ws pats) pat@(name, xs) = H n (Map.mapWithKey f ws) (pat:pats)
  where f (i, j) w = w + xs !! i * xs !! j

input :: Hopfield -> State -> (Hopfield, State)
input = (,)

net :: (Hopfield, State) -> Int -> Int
net (h@(H n _ _), xs) i = sum [weight h i j * xs `si` j | j <- [0..n-1]]

update :: (Hopfield, State) -> Int -> (Hopfield, State)
update (h@(H n _ _), xs) i = (h, Seq.update i nx xs)
  where nx = if net (h, xs) i > 0 then 1 else -1

empty :: Int -> Hopfield
empty numNeurons = H numNeurons (Map.fromList $ zip pairs (repeat 0)) []
  where pairs = [(i, j) | i <- neurons, j <- neurons, i < j]
        neurons = [0..numNeurons - 1]

energy :: (Hopfield, State) -> Float
energy (h@(H _ ws _), xs) = Map.foldWithKey f 0 ws
  where f (i, j) _ s = s - fromIntegral
                       (weight h i j * xs `si` i * xs `si` j)

energy2 :: (Hopfield, State) -> Float
energy2 (h@(H n ws _), xs) = (-1/2) * fromIntegral
  (sum [weight h i j * xs `si` i * xs `si` j | i <- [0..n-1], j <- [0..n-1]])

updateAll :: (Hopfield, State) -> IO (Hopfield, State)
updateAll (h@(H n _ _), xs) = do
  updateOrder <- shuffle [0..n-1]
  return $ foldl update (h, xs) updateOrder
