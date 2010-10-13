module Hopfield where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Shuffle

type Pattern = ([Int], String)

data Hopfield = H Int (Map (Int, Int) Int) [Pattern]
  deriving (Eq, Read, Show)

type State = Seq Int

type NetState = (Hopfield, State)

si = Seq.index

save :: FilePath -> Hopfield -> IO ()
save filepath h = writeFile filepath (show h)

load :: FilePath -> IO Hopfield
load filepath = readFile filepath >>= return . read

numNeurons :: Hopfield -> Int
numNeurons (H n _ _) = n

-- 'weight h i j' is the weight between neurons 'i' and 'j' in network 'h'
weight :: Hopfield -> Int -> Int -> Int
weight (H _ ws _) i j | i == j = 0
                      | i < j = ws Map.! (i, j)
                      | i > j = ws Map.! (j, i)

-- 'add h pat' stores 'pat' in network 'h', resulting in new network
add :: Hopfield -> Pattern -> Hopfield
add (H n ws pats) pat@(xs, name) = H n (Map.mapWithKey f ws) (pat:pats)
  where f (i, j) w = w + xs !! i * xs !! j

-- 'input h pat' inputs 'pat' to network 'h' to create an initial NetState
input :: Hopfield -> [Int] -> NetState
input h pat = (h, Seq.fromList pat)

-- 'net i s' is net (from book) of neuron 'i' in NetState 's'
net :: Int -> NetState -> Int
net i (h@(H n _ _), xs) = sum [weight h i j * xs `si` j | j <- [0..n-1]]

-- 'update i s' updates neuron 'i' in NetState 's', resulting in new NetState
update :: Int -> NetState -> NetState
update i (h@(H n _ _), xs) = (h, Seq.update i nx xs)
  where nx | neti > 0 = 1
           | neti == 0 = 0
           | neti < 0 = -1
        neti = net i (h, xs)

-- 'empty numNeurons' creates an empty (0-weight) network with 'numNeurons'
-- neurons
empty :: Int -> Hopfield
empty numNeurons = H numNeurons (Map.fromList $ zip pairs (repeat 0)) []
  where pairs = [(i, j) | i <- neurons, j <- neurons, i < j]
        neurons = [0..numNeurons - 1]

energy :: NetState -> Float
energy (h@(H _ ws _), xs) = Map.foldWithKey f 0 ws
  where f (i, j) _ s = s - fromIntegral
                       (weight h i j * xs `si` i * xs `si` j)

-- equivalent to 'energy' above, expressed more like the book formula
--energy2 :: NetState -> Float
--energy2 (h@(H n ws _), xs) = (-1/2) * fromIntegral
--  (sum [weight h i j * xs `si` i * xs `si` j | i <- [0..n-1], j <- [0..n-1]])

convergence :: NetState -> Maybe String
convergence (H _ _ pats, s) = lookup (toList s) pats
