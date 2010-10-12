module Hopfield where

type Pattern = (String, [Int])

data Hopfield = Weights [[Int]] [Pattern]
  deriving (Read, Show)

-- (the network, the current values)
data State = Values [Int]
  deriving (Read, Show)

add :: Hopfield -> Pattern -> Hopfield
add (Weights w pats) pat@(name, x) = Weights nw (pat:pats)
  where nw = do i <- [0..length w]
                return $ do j <- [0..length (w !! i)]
                            let wij = w !! i !! j
                                nwij = wij + x !! i * x !! j
                            return nwij

input :: Hopfield -> State -> (Hopfield, State)
input = (,)

update :: Int -> (Hopfield, State) -> (Hopfield, State)
update neuron (Weights w p, Values x) = (Weights w p, Values x)

zeroNetwork :: Int -> Hopfield
zeroNetwork numNeurons = Weights (replicate numNeurons (replicate numNeurons 0)) []
