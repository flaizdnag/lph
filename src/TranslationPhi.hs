{-|
Module      : TranslationPhi
Description : Functions that allow to translate a logic program into a neural
              network.
Copyright   : (c) Aleksandra Cz., 2019
                  Marta G., 2019
                  Kinga O., 2019
                  Agata T., 2019
License     : GPL-3length
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module TranslationPhi
    ( ) where

import Auxiliary
import NeuralNetworks
import LogicPrograms
import Data.List (length, maximum, map, find, (\\), delete, partition, foldl1)
import Data.Char 
import System.Random

