{-|
Module      : Translation
Description : 
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
    ( Neuron (..)
    , Connection (..)
    , NeuralNetwork (..)
    , bodyLength
    , bodiesLength
    , sameHeadsCl
    , sameHeadsLP
    , amin
    , w
    , biasHidN
    , biasOutN
    , makeInpN
    , inputNeurons
    , makeOutN
    , outputNeurons
    ) where

import LogicPrograms
import Data.List (length, maximum, map)


-- | 
-- 

data Neuron = Neuron 
    { label :: String 
    , actF :: Int 
    , bias :: Float 
    , idx :: String 
    }

instance Show Neuron where 
    show (Neuron label actF bias idx) = "(" ++ label ++ ", " ++ show actF ++ ", " ++ show bias ++ ", " ++ idx ++ ")"

data Connection = Connection
    { from :: String
    , to :: String
    , weight :: Float
    }

instance Show Connection where
    show (Connection from to w) = "(" ++ from ++ ", " ++ to ++ ", " ++ show w ++ ")"

data NeuralNetwork = NN
    { neurons :: [Neuron]
    , connections :: [Connection]
    }

instance Show NeuralNetwork where
    show (NN ns cs) = "(" ++ show ns ++ ", " ++ show cs ++ ")"

-- | Function that returns length of the body of a given Horn clause.
bodyLength :: Clause -> Int 
bodyLength = length . clBody

-- | Function that returns lengths of all bodies of Horn clauses from a given
-- logic program. 
bodiesLength :: LP -> [Int] 
bodiesLength = map bodyLength

-- | Function that returns the number of Horn clauses that have the same atom
-- in their head as the given Horn clause.
sameHeadsCl :: Clause -> LP -> Int 
sameHeadsCl cl lp = length [ cls | cls <- lp, clHead cls == clHead cl ]

-- | Function that returns the number of Horn clauses that have the same atom
-- in their head as the given Horn clause for every Horn clause in a given 
-- logic program. 
sameHeadsLP :: LP -> [Int]
sameHeadsLP lp = map (\x -> sameHeadsCl x lp) lp

-- | Function that returns a value A_min, i.e. 
amin :: LP -> Float
amin lp = (fromIntegral (m - 1) / fromIntegral (m + 1)) + 0.1
        where
            m = maximum $ bodiesLength lp ++ sameHeadsLP lp

-- | Function that returns weight of the connections in neural network for 
-- a given logic program. 
w :: LP -> Float
w lp = (2 / b) * (((log $ 1 + amin lp) - (log $ 1 - amin lp)) / ((fromIntegral m) * (amin lp - 1) + amin lp + 1))
        where
            b = 1
            m = maximum $ bodiesLength lp ++ sameHeadsLP lp

-- | Function that returns a bias of a hidden layer neuron for a given Horn
-- clause and logic program.
biasHidN :: Clause -> LP -> Float
biasHidN cl lp = (((1 + amin lp) * (fromIntegral (bodyLength cl) - 1)) / 2) * w lp

-- | Function that returns a bias of an output layer neuron for a given Horn
-- clause and logic program.
biasOutN :: Clause -> LP -> Float
biasOutN cl lp = (((1 + amin lp) * (1 - fromIntegral (sameHeadsCl cl lp))) / 2) * w lp

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

p :: LP
p = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 1 "") [A 3 ""] [], Fact (A 5 "")]