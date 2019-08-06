{-|
Module      : NeuralNetworks
Description : Definitions of types that concern neural networks.
Copyright   : (c) Aleksandra Cz., 2019
                  Kinga O., 2019
                  Andrzej G., 2019
License     : GPL-3length
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module NeuralNetworks
    ( Neuron (..)
    , Connection (..)
    , NeuralNetwork (..)
    , NNupdate (..)
    , NNfactors (..)
    , neuronsToPythonString
    , nnToPythonString
    , truthNN
    , emptyNN
    , emptyNNupd
    ) where

import Data.List (intercalate)


data Neuron = Neuron 
    { label     :: String 
    , activFunc :: String 
    , bias      :: Float 
    , idx       :: String 
    }
    deriving (Read, Eq)

instance Show Neuron where 
    show (Neuron l aF b idx) = "(" ++ l ++ ", " ++ aF ++ ", " ++ show b ++ ", " ++ idx ++ ")"


data Connection = Connection
    { from   :: String
    , to     :: String
    , weight :: Float
    }
    deriving (Read, Eq)

instance Show Connection where
    show (Connection from to w) = "(" ++ from ++ ", " ++ to ++ ", " ++ show w ++ ")"


data NeuralNetwork = NN
    { inpLayer            :: [Neuron]
    , hidLayer            :: [Neuron]
    , outLayer            :: [Neuron]
    , recLayer            :: [Neuron]
    , inpToHidConnections :: [Connection]
    , hidToOutConnections :: [Connection]
    , recConnections      :: [Connection]
    }
    deriving (Show, Read)


-- | A special type for neural networks updates handling.
data NNupdate = NNupdate
    { inpNeuToAdd      :: [Neuron]
    , hidNeuToAdd      :: [Neuron]
    , outNeuToAdd      :: [Neuron]
    , outNeuToRemove   :: [Neuron]
    , inpToHidConToAdd :: [Connection]
    , hidToOutConToAdd :: [Connection]
    }
    deriving (Show, Read)

data NNfactors = NNfactors
    { beta            :: Float  -- beta coefficient for sigmoid function
    , addHidNeuNumber :: Int    -- number of additional hidden layer neurons
    , addWeightLimit  :: Float  -- maximal value of an additional connection weight
    , addNeuronsBias  :: Float  -- bias for additional neurons    
    , weightFactor    :: Float  -- weight W factor (added value)
    , aminFactor      :: Float  -- A_min factor (added value)
    }


neuronsToPythonString :: [Neuron] -> String
neuronsToPythonString ns = "[" ++ intercalate ", " stringList ++ "]"
    where
        stringList = do
            (Neuron label activFunc bias idx) <- ns
            return ("(" ++ show label ++ ", " ++ show activFunc ++ ", " ++ show bias ++ ", " ++ show idx ++ ")")


connectionsToPythonString :: [Connection] -> String
connectionsToPythonString ns = "[" ++ intercalate ", " stringList ++ "]"
    where
        stringList = do
            (Connection from to weight) <- ns
            return ("(" ++ show from ++ ", " ++ show to ++ ", " ++ show weight ++ ")")


showConns :: [Connection] -> String
showConns []                          = "]"
showConns ((Connection from to w):xs) = case length ((Connection from to w):xs) of 
    1 -> "(" ++ show from ++ ", " ++ show to ++ ", " ++ show w ++ ")]"
    _ -> "(" ++ show from ++ ", " ++ show to ++ ", " ++ show w ++ "), " ++ showConns xs


nnToPythonString :: NeuralNetwork -> String
nnToPythonString (NN iL hL oL rL ihC hoC rC) =
    "\"inpLayer\" = " ++ neuronsToPythonString iL ++ "\n" ++
    "\"hidLayer\" = " ++ neuronsToPythonString hL ++ "\n" ++
    "\"outLayer\" = " ++ neuronsToPythonString oL ++ "\n" ++
    "\"recLayer\" = " ++ neuronsToPythonString rL ++ "\n" ++
    "\"inpToHidConnections\" = " ++ connectionsToPythonString ihC ++ "\n" ++
    "\"hidToOutConnections\" = " ++ connectionsToPythonString hoC ++ "\n" ++
    "\"recConnections\" = " ++ connectionsToPythonString rC

{-
nnToPythonString' :: NeuralNetwork -> String
nnToPythonString' (NN iL hL oL rL ihC hoC rC) =
    "{\"inpLayer\" = [" ++ showNs iL ++ 
    "\"hidLayer\" = [" ++ showNs hL ++ 
    "\"outLayer\" = [" ++ showNs oL ++ 
    "\"recLayer\" = [" ++ showNs rL ++ 
    "\"inpToHidConnections\" = [" ++ showConns ihC ++ 
    "\"hidToOutConnections\" = [" ++ showConns hoC ++ 
    "\"recConnections\" = [" ++ showConns rC ++ "}"
-}


truthNN :: Float -> NNupdate
truthNN w = NNupdate
    { inpNeuToAdd      = [Neuron "inpT" "const" 0.0 "inpT"]
    , hidNeuToAdd      = [Neuron "hidT" "tanh" 0.0 "hidT"]
    , outNeuToAdd      = []
    , outNeuToRemove   = []
    , inpToHidConToAdd = [Connection "inpT" "hidT" w]
    , hidToOutConToAdd = []
    }


emptyNNupd :: NNupdate
emptyNNupd = NNupdate
    { inpNeuToAdd      = []
    , hidNeuToAdd      = []
    , outNeuToAdd      = []
    , outNeuToRemove   = []
    , inpToHidConToAdd = []
    , hidToOutConToAdd = []
    }


emptyNN :: NeuralNetwork
emptyNN = NN
    { inpLayer            = []
    , hidLayer            = []
    , outLayer            = []
    , recLayer            = []
    , inpToHidConnections = []
    , hidToOutConnections = []
    , recConnections      = []
    }
