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
    , showNNPython
    , showNs
    , showConns
    ) where


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
    deriving (Read)

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
--    , addConnections      :: [Connection]
    }
    deriving (Show, Read)


showNNPython :: NeuralNetwork -> IO()
showNNPython (NN iL hL oL rL ihC hoC rC) =
    mapM_ putStrLn ["{\"inpLayer\" = [" ++ showNs iL ++ 
                    "\"hidLayer\" = [" ++ showNs hL ++ 
                    "\"outLayer\" = [" ++ showNs oL ++ 
                    "\"recLayer\" = [" ++ showNs rL ++ 
                    "\"inpToHidConnections\" = [" ++ showConns ihC ++ 
                    ", \"hidToOutConnections\" = [" ++ showConns hoC ++ 
                    ", \"recConnections\" = [" ++ showConns rC ++ "}"]

showNs :: [Neuron] -> String
showNs []                       = "], "
showNs ((Neuron l aF b idx):xs) = case length ((Neuron l aF b idx):xs) of 
    1 -> "(" ++ show l ++ ", " ++ show aF ++ ", " ++ show b ++ ", " ++ show idx ++ ")], "
    _ -> "(" ++ show l ++ ", " ++ show aF ++ ", " ++ show b ++ ", " ++ show idx ++ "), " ++ showNs xs

showConns :: [Connection] -> String
showConns []                          = "]"
showConns ((Connection from to w):xs) = case length ((Connection from to w):xs) of 
    1 -> "(" ++ show from ++ ", " ++ show to ++ ", " ++ show w ++ ")]"
    _ -> "(" ++ show from ++ ", " ++ show to ++ ", " ++ show w ++ "), " ++ showConns xs

