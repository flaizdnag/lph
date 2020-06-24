{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
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
    , printNNPythonString
    , saveToFile
    ) where

import Data.Aeson
import Data.Map
import Data.Text
import GHC.Generics

import Auxiliary
import Data.List (intercalate)
import System.IO  


data Neuron = Neuron 
    { label     :: String 
    , activFunc :: String 
    , bias      :: Float 
    , idx       :: String 
    }
    deriving (Generic, Read, Eq)

instance FromJSON Neuron
instance ToJSON Neuron where
    toEncoding = genericToEncoding defaultOptions

instance Show Neuron where 
    show (Neuron l aF b idx) = "(" ++ l ++ ", " ++ aF ++ ", " ++ show b ++ ", " ++ idx ++ ")"

instance Ord Neuron where
    n1 < n2 = idx n1 < idx n2
    
    a <= b = (a < b) || (a == b)
    a >  b = b < a
    a >= b = b <= a


data Connection = Connection
    { fromNeuron :: String
    , toNeuron   :: String
    , weight     :: Float
    }
    deriving (Generic, Read, Eq)

instance FromJSON Connection
instance ToJSON Connection where
    toEncoding = genericToEncoding defaultOptions

instance Show Connection where
    show (Connection from to w) = "(" ++ from ++ ", " ++ to ++ ", " ++ show w ++ ")"

instance Ord Connection where
    c1 < c2 = fromNeuron c1 < fromNeuron c2
    
    a <= b = (a < b) || (a == b)
    a >  b = b < a
    a >= b = b <= a


data NeuralNetwork = NN
    { inpLayer            :: [Neuron]
    , hidLayer            :: [Neuron]
    , outLayer            :: [Neuron]
    , recLayer            :: [Neuron]
    , inpToHidConnections :: [Connection]
    , hidToOutConnections :: [Connection]
    , recConnections      :: [Connection]
    }
    deriving (Generic, Show, Read)

instance FromJSON NeuralNetwork
instance ToJSON NeuralNetwork where
    toEncoding = genericToEncoding defaultOptions

instance Eq NeuralNetwork where
    nn1 == nn2 =
        Prelude.all (True==)
            [ eqLists (inpLayer nn1) (inpLayer nn2)
            , eqLists (hidLayer nn1) (hidLayer nn2)
            , eqLists (outLayer nn1) (outLayer nn2)
            , eqLists (recLayer nn1) (recLayer nn2)
            ]
        
        &&
        
        Prelude.all (True==)
            [ eqLists (inpToHidConnections nn1) (inpToHidConnections nn2)
            , eqLists (hidToOutConnections nn1) (hidToOutConnections nn2)
            , eqLists (recConnections nn1) (recConnections nn2)
            ]


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
    deriving (Show, Read)


neuronsToPythonString :: [Neuron] -> String
neuronsToPythonString ns = "[" ++ Data.List.intercalate ", " stringList ++ "]"
    where
        stringList = do
            (Neuron label activFunc bias idx) <- ns
            return ("(" ++ show label ++ ", " ++ show activFunc ++ ", " ++ show bias ++ ", " ++ show idx ++ ")")


connectionsToPythonString :: [Connection] -> String
connectionsToPythonString ns = "[" ++ Data.List.intercalate ", " stringList ++ "]"
    where
        stringList = do
            (Connection from to weight) <- ns
            return ("(" ++ show from ++ ", " ++ show to ++ ", " ++ show weight ++ ")")


nnToPythonString :: NeuralNetwork -> String
nnToPythonString (NN iL hL oL rL ihC hoC rC) =
    "{\"inpLayer\" = " ++ neuronsToPythonString iL ++ ", " ++
    "\"hidLayer\" = " ++ neuronsToPythonString hL ++ ", " ++
    "\"outLayer\" = " ++ neuronsToPythonString oL ++ ", " ++
    "\"recLayer\" = " ++ neuronsToPythonString rL ++ ", " ++
    "\"inpToHidConnections\" = " ++ connectionsToPythonString ihC ++ ", " ++
    "\"hidToOutConnections\" = " ++ connectionsToPythonString hoC ++ ", " ++
    "\"recConnections\" = " ++ connectionsToPythonString rC ++ "}"


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


printNNPythonString :: IO NeuralNetwork -> IO String
printNNPythonString inp = do
    nn <- inp
    return $ nnToPythonString nn


saveToFile :: IO String -> IO ()
saveToFile nn = do
    toWrite <- nn
    writeFile "NN_new.txt" toWrite






exmplNN :: NeuralNetwork
exmplNN = NN
    { inpLayer            = [Neuron "A1" "id" 0.0 "inp1", Neuron "A2" "id" 0.0 "inp2"]
    , hidLayer            = [Neuron "h1" "tanh" 1.0 "hid1", Neuron "h2" "tanh" 2.0 "hid2"]
    , outLayer            = [Neuron "A1" "tanh" 1.0 "out1", Neuron "A2" "tanh" 2.0 "out2"]
    , recLayer            = [Neuron "rec1" "special" 1.0 "rec1"]
    , inpToHidConnections = [Connection "inp1" "hid2" 1.0, Connection "inp2" "hid1" 2.0]
    , hidToOutConnections = [Connection "hid2" "out2" 1.0, Connection "hid1" "out1" 2.0]
    , recConnections      = [Connection "out2" "rec1" 1.0, Connection "out1" "rec1" 1.0, Connection "rec1" "inp1" 1.0]
    }
