{-|
Module      : Translation
Description : Functions that allow to translate a logic program into a neural
              network.
Copyright   : (c) Aleksandra Cz., 2019
                  Kinga O., 2019
                  Andrzej G., 2019
License     : GPL-3length
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module Translation
    ( ) where

import NeuralNetworks
import LogicPrograms
import Data.List (length, maximum, map)



-- | Function that returns length of the body of a given Horn clause.
bodyLength :: Clause -> Int 
bodyLength = length . clBody

-- | Function that returns lengths of all bodies of Horn clauses from a given
-- logic program. 
bodiesLength :: LP -> [Int] 
bodiesLength = map bodyLength

-- | Function that returns the number of Horn clauses that have the same atom
-- in their head as the given Horn clause.
clSameHeads :: Clause -> LP -> Int 
clSameHeads cl lp = length [ cls | cls <- lp, clHead cls == clHead cl ]

-- | Function that returns the number of Horn clauses that have the same atom
-- in their head as the given Horn clause for every Horn clause in a given 
-- logic program. 
clsSameHeads :: LP -> [Int]
clsSameHeads lp = map (\x -> clSameHeads x lp) lp

-- | Function that returns the base for the value A_min. 
aminBase :: LP -> Float
aminBase lp = (fromIntegral (m - 1) / fromIntegral (m + 1))
    where
        m = maximum $ bodiesLength lp ++ clsSameHeads lp

-- | Function that returns weight of the connections in neural network for 
-- a given logic program. 
wBase :: LP -> Float -> Float -> Int -> Float
wBase lp aminF r l = maximum [(2 / beta) * (((log $ 1 + amin) - (log $ 1 - amin)) / ((fromIntegral maxBodies) * (amin - 1) + amin + 1)), (2 / beta) * (((log $ 1 + amin) - (log $ 1 - amin) - (r * (fromIntegral $ l + 1)) ) / ((fromIntegral maxHeads) * (amin - 1) + amin + 1))]
    where
        beta      = 1
        amin      = aminBase lp + aminF
        maxBodies = maximum $ bodiesLength lp
        maxHeads  = maximum $ clsSameHeads lp

-- | Function that returns a bias of a hidden layer neuron for a given Horn
-- clause and logic program.
biasHidN :: Clause -> LP -> Float
biasHidN cl lp = (((1 + amin lp) * (fromIntegral (bodyLength cl) - 1)) / 2) * w lp

-- | Function that returns a bias of an output layer neuron for a given Horn
-- clause and logic program.
biasOutN :: Clause -> LP -> Float
biasOutN cl lp = (((1 + amin lp) * (1 - fromIntegral (sameHeadsCl cl lp))) / 2) * w lp

{-
-- | Function that converts an atom into an input layer neuron. 
makeInpN :: Atom -> Int -> Neuron
makeInpN (A idx lab) n = Neuron {Translation.label = "a" ++ show idx, actF = 1, bias = 0, Translation.idx = "I" ++ show n}

-- | Function that converts an atom into an output layer neuron. 
makeOutN :: Atom -> Int -> Float -> Neuron
makeOutN (A idx lab) n b = Neuron {Translation.label = "a" ++ show idx, actF = 2, bias = b, Translation.idx = "O" ++ show n}

-- | Function that returns a list of the input layer neurons in the neural 
-- network for a given logic program. 
inputNeurons :: LP -> [Neuron]
inputNeurons lp = zipWith makeInpN (bp lp) [1..]

-- | Function that returns a list of the output layer neurons in the neural
-- network for a given logic program. 
outputNeurons :: LP -> [Neuron]
outputNeurons lp = zipWith3 makeOutN (bp lp) [1..] bs
        where 
            bs = [ biasOutN cl lp | cl <- lp ] ++ [ 0 | _ <- onlyBodies lp ]
-}

p1 :: LP
p1 = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 1 "") [A 3 ""] [], Fact (A 5 "")]
