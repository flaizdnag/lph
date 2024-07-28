{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      : JsonHandling
Description : Jsons handling
Copyright   : (c) Andrzej G., 2017-
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Long description.
-}
module JsonHandling (
    LPjson (..),
    Factors (..),
    LPtoNN (..),
    LPtoNNnoABD (..),
    NNwithFactors (..),
    lpjosnTOlp,
    factorsTOnnfactors,
    NNwithAmin (..),
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text
import GHC.Generics

import LogicPrograms
import qualified NeuralNetworks as NN

data LPjson = LPjson
    { facts :: [Clause]
    , assumptions :: [Clause]
    , clauses :: [Clause]
    }
    deriving (Show, Read, Eq, Generic, Ord)

instance FromJSON LPjson
instance ToJSON LPjson where
    toEncoding = genericToEncoding defaultOptions

data Factors = Factors
    { beta_for_activation_function :: Float
    , number_of_additional_neurons :: Float
    , additional_weights_range :: Float
    , bias_for_additional_neurons :: Float
    , w_factor :: Float
    , amin_factor :: Float
    }
    deriving (Show, Read, Generic)

instance FromJSON Factors
instance ToJSON Factors where
    toEncoding = genericToEncoding defaultOptions

data LPtoNN = LPtoNN
    { lp :: LPjson
    , abductive_goal :: Clause
    , factors :: Factors
    }
    deriving (Show, Read, Generic)

instance FromJSON LPtoNN
instance ToJSON LPtoNN where
    toEncoding = genericToEncoding defaultOptions

data LPtoNNnoABD = LPtoNNnoABD
    { lpnoABD :: LPjson
    , factorsnoABD :: Factors
    }
    deriving (Show, Read, Generic)

instance FromJSON LPtoNNnoABD
instance ToJSON LPtoNNnoABD where
    toEncoding = genericToEncoding defaultOptions

data NNwithFactors = NNwithFactors
    { nn :: NN.NeuralNetwork
    , nnFactors :: Factors
    }
    deriving (Show, Read, Generic)

instance FromJSON NNwithFactors
instance ToJSON NNwithFactors where
    toEncoding = genericToEncoding defaultOptions

data NNwithAmin = NNwithAmin
    { neuralNetwork :: NN.NeuralNetwork
    , amin :: Float
    }
    deriving (Show, Read, Generic)

instance FromJSON NNwithAmin
instance ToJSON NNwithAmin where
    toEncoding = genericToEncoding defaultOptions

lpjosnTOlp :: LPjson -> LP
lpjosnTOlp (LPjson fs as cls) = fsNew ++ asNew ++ cls
  where
    fsNew = Prelude.map (Fact . clHead) fs
    asNew = Prelude.map (Assumption . clHead) as

factorsTOnnfactors :: Factors -> NN.NNfactors
factorsTOnnfactors f =
    NN.NNfactors
        { NN.beta = beta_for_activation_function f
        , NN.addHidNeuNumber = round $ number_of_additional_neurons f
        , NN.addWeightLimit = additional_weights_range f
        , NN.addNeuronsBias = bias_for_additional_neurons f
        , NN.weightFactor = w_factor f
        , NN.aminFactor = amin_factor f
        }
