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
    ) where

import LogicPrograms
import Auxiliary
import Data.List (partition)
import Data.Aeson
import GHC.Generics



data InpOutTOlp = InpOutTOlp
    { orderInp :: [Atom]
    , orderOut :: [Atom]
    , amin     :: Float
    , ioPairs  :: [IOpair]
    } deriving (Generic, Read, Eq)

instance FromJSON InpOutTOlp
instance ToJSON InpOutTOlp where
    toEncoding = genericToEncoding defaultOptions



type IOpair   = ([Int], [Float])



decodeNN :: InpOutTOlp -> LP
decodeNN (InpOutTOlp inpAtoms outAtoms amin ioPairs) = do
    (inpValues, outValues) <- ioPairs
    
    let ((trAtoms, faAtoms), heads) =
            (getAtoms $ atomsTrFa $ zip inpAtoms inpValues, trHeads $ zip outAtoms outValues)
        
        getAtoms (p1, p2) = (map fst p1, map fst p2)
        atomsTrFa = partition ((1==) . snd) . filter (("truth"/=) . label . fst)
        trHeads = map fst . filter ((amin<=) . snd)
    
    clause <- do
        head <- heads
        return (Cl head trAtoms faAtoms)
    
    return clause
