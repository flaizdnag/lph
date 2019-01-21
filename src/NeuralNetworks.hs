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
    ) where


data Neuron = Neuron 
    { label     :: String 
    , activFunc :: String 
    , bias      :: Float 
    , idx       :: String 
    }

instance Show Neuron where 
    show (Neuron l aF b idx) = "(" ++ l ++ ", " ++ aF ++ ", " ++ show b ++ ", " ++ idx ++ ")"



data Connection = Connection
    { from   :: String
    , to     :: String
    , weight :: Float
    }

instance Show Connection where
    show (Connection from to w) = "(" ++ from ++ ", " ++ to ++ ", " ++ show w ++ ")"



data NeuralNetwork = NN
    { neurons     :: [Neuron]
    , connections :: [Connection]
    }

instance Show NeuralNetwork where
    show (NN ns cs) = "(" ++ show ns ++ ", " ++ show cs ++ ")"
