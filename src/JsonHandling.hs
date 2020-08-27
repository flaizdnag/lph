{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : JsonHandling
Description : Jsons handling
Copyright   : (c) Andrzej G., 2017-
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Long description.
-}
module JsonHandling
    ( LPjson (..)
    , Factors (..)
    , LPtoNN (..)
    , NNwithFactors (..)
    , lpjosnTOlp
    , factorsTOnnfactors
    ) where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.Text

import LogicPrograms
import qualified NeuralNetworks as NN


data LPjson = LPjson
    { facts       :: [Clause]
    , assumptions :: [Clause]
    , clauses     :: [Clause]
    } deriving (Show, Read, Generic)

instance FromJSON LPjson
instance ToJSON LPjson where
    toEncoding = genericToEncoding defaultOptions



data Factors = Factors
    { beta :: Float
    , ahln :: Float
    , r    :: Float
    , bias :: Float
    , w    :: Float
    , amin :: Float
    } deriving (Show, Read, Generic)

instance FromJSON Factors
instance ToJSON Factors where
    toEncoding = genericToEncoding defaultOptions



data LPtoNN = LPtoNN
    { lp             :: LPjson
    , abductive_goal :: Clause
    , factors        :: Factors
    } deriving (Show, Read, Generic)

instance FromJSON LPtoNN
instance ToJSON LPtoNN where
    toEncoding = genericToEncoding defaultOptions



data NNwithFactors = NNwithFactors
    { nn        :: NN.NeuralNetwork
    , nnFactors :: Factors
    } deriving (Show, Read, Generic)

instance FromJSON NNwithFactors
instance ToJSON NNwithFactors where
    toEncoding = genericToEncoding defaultOptions



lpjosnTOlp :: LPjson -> LP
lpjosnTOlp (LPjson fs as cls) = fsNew ++ asNew ++ cls
    where
        fsNew = Prelude.map (\x -> Fact (clHead x)) fs
        asNew = Prelude.map (\x -> Assumption (clHead x)) as


factorsTOnnfactors :: Factors -> NN.NNfactors
factorsTOnnfactors f = NN.NNfactors 
    { NN.beta            = JsonHandling.beta f
    , NN.addHidNeuNumber = round $ ahln f
    , NN.addWeightLimit  = r f
    , NN.addNeuronsBias  = JsonHandling.bias f
    , NN.weightFactor    = w f
    , NN.aminFactor      = amin f
    }
