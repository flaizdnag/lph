{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : NNdecoder
Description : Conversion from Python data into a logic program.
Copyright   : (c) Andrzej G., 2017-
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Longer description.
-}
module NNdecoder
    ( InpOutTOlp (..)
    , decodeNN
    , neuronToAtom
    ) where

import LogicPrograms as LP
import NeuralNetworks as NN
import Auxiliary
import Data.List (partition)
import Data.Aeson
import GHC.Generics



data InpOutTOlp = InpOutTOlp
    { orderInp :: [String]
    , orderOut :: [String]
    , amin     :: Float
    , ioPairs  :: [IOpair]
    } deriving (Generic, Read, Eq)

instance FromJSON InpOutTOlp
instance ToJSON InpOutTOlp where
    toEncoding = genericToEncoding defaultOptions



type IOpair   = ([Int], [Float])


neuronToAtom :: String -> Atom
neuronToAtom n = case n of
	"inpT" -> A { LP.idx = -1, LP.label = "truth" }
	_      -> A { LP.idx = newIndex, LP.label = newLabel}
    		where
			newIndex = read $ tail $ n
			newLabel = []

decodeNN :: InpOutTOlp -> LP
decodeNN (InpOutTOlp inpNs outNs amin ioPairs) = do
    (inpValues, outValues) <- ioPairs
    
    let inpAtoms = map neuronToAtom inpNs
        outAtoms = map neuronToAtom outNs
        ((trAtoms, faAtoms), heads) =
            (getAtoms $ atomsTrFa $ zip inpAtoms inpValues, trHeads $ zip outAtoms outValues)
        
        getAtoms (p1, p2) = (map fst p1, map fst p2)
        atomsTrFa = partition ((1==) . snd) . filter (("truth"/=) . LP.label . fst)
        trHeads = map fst . filter ((amin<=) . snd)
    
    clause <- do
        head <- heads
        return (Cl head trAtoms faAtoms)
    
    return clause
