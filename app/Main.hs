{-# LANGUAGE OverloadedStrings #-}

module Main where

import Acceptable
import Completion
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (Text, pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import JsonHandling
import LPsimplifier
import LogicPrograms
import NNdecoder
import NeuralNetworks
import Text.ParserCombinators.ReadP (readP_to_S)
import TranslationTp
import Web.Scotty (ActionM, body, json, liftAndCatchIO, post, scotty, text)

main :: IO ()
main = scotty 10100 $ do
    {-
        post "/api/bP" $ do
            b <- decodeUtf8 <$> body
            responseLP b bp

        post "/api/positiveBodies" $ do
            b <- decodeUtf8 <$> body
            responseLP b lpPBodies

        post "/api/negativeBodies" $ do
            b <- decodeUtf8 <$> body
            responseLP b lpNBodies

        post "/api/completion" $ do
            b <- decodeUtf8 <$> body
            responseLP b comp

        post "/api/isAcceptable" $ do
            b <- decodeUtf8 <$> body
            responseLP b isAcceptable

        post "/api/getNN" $ do
            b <- decodeUtf8 <$> body
            responseNN b
    -}

    post "/api/abdAt" $ do
        b <- decodeUtf8 <$> body
        getNN_atomsAbd b

    post "/api/abdCl" $ do
        b <- decodeUtf8 <$> body
        getNN_clauseAbd b

    post "/api/nn2lp" $ do
        b <- body
        nn2lp b

    post "/api/lp2nn" $ do
        b <- body
        lp2nn b

    post "/api/lp2nnNoAbd" $ do
        b <- body
        lp2nn_no_abd b

    post "/api/lp2nnBase" $ do
        b <- body
        lp2nn_base b

{-
responseLP :: Show a => Text -> (LP -> a) -> Web.Scotty.ActionM ()
responseLP input function = textPackShow $ function $ readUnpackLP input
    where
        readUnpackLP x = read (unpack x) :: LP
        textPackShow x = text $ pack $ show x

responseString :: Text -> (String -> String) -> Web.Scotty.ActionM ()
responseString input function = textPackShow $ function $ unpack input
    where
        textPackShow x  = text $ pack x

responseNN :: Text -> Web.Scotty.ActionM ()
responseNN input = do
    converted <- liftAndCatchIO nnPythonString
    text . pack $ converted
        where
            inputLP   = read (unpack input) :: LP
            nnFactors = NNfactors 1 1 0.05 0.0 0.5 0.5
            nnBase    = baseNN inputLP nnFactors
            nnAdd     = additionalNN nnBase nnFactors []
            nnFull    = do
                nn <- nnAdd
                return $ recursiveConnections nn (overlappingAtoms inputLP [])
            nnPythonString = do
                nn <- nnFull
                return $ nnToPythonString nn
-}

getNN_atomsAbd :: Text -> Web.Scotty.ActionM ()
getNN_atomsAbd input = do
    converted <- liftAndCatchIO nnPythonString
    text . pack $ converted
  where
    div = lines $ unpack input
    inpLP = read (div !! 0) :: LP
    abd = read (div !! 1) :: [Atom]
    nnFac = floatsToNNfac (read (div !! 2) :: [Float])
    (nnBase, amin) = baseNN inpLP nnFac
    nnAdd = additionalNN nnBase nnFac abd
    nnFull = do
        nn <- nnAdd
        return $ recursiveConnections nn (overlappingAtoms inpLP abd)
    nnPythonString = do
        nn <- nnFull
        return $ nnToPythonString nn

getNN_clauseAbd :: Text -> Web.Scotty.ActionM ()
getNN_clauseAbd input = do
    converted <- liftAndCatchIO nnFull
    text $ pack $ show converted
  where
    div = lines $ unpack input
    inpLP = read (div !! 0) :: LP
    cl = read (div !! 1) :: Clause
    abd = [clHead cl]
    nnFac = floatsToNNfac (read (div !! 2) :: [Float])
    modLP = modifiedLP inpLP cl
    (nnBase, amin) = baseNN modLP nnFac
    nnAdd = additionalNN nnBase nnFac abd
    nnFull = do
        nn <- nnAdd
        return $ (recursiveConnections nn (overlappingAtoms modLP abd), amin)

floatsToNNfac :: [Float] -> NNfactors
floatsToNNfac (a : b : c : d : e : f : xs) =
    NNfactors
        { NeuralNetworks.beta = a
        , addHidNeuNumber = round b
        , addWeightLimit = c
        , addNeuronsBias = d
        , weightFactor = e
        , aminFactor = f
        }

nn2lp input = case (decode input :: Maybe InpOutTOlp) of
    Nothing -> text $ pack "Problems with nn2lp json"
    Just decodedInp -> Web.Scotty.json finalLP
      where
        finalLP = makeLPpython $ simplifyLP $ decodeNN decodedInp

lp2nn input = case (decode input :: Maybe LPtoNN) of
    Nothing -> text $ pack "Problems with lp2nn json"
    Just decodedInp -> do
        converted <- liftAndCatchIO nnWfac
        Web.Scotty.json converted
      where
        inpLP = lpjosnTOlp $ JsonHandling.lp decodedInp
        cl = abductive_goal decodedInp
        abd = [clHead cl]
        nnFac = factorsTOnnfactors $ factors decodedInp
        modLP = modifiedLP (mkLPpositive inpLP) cl
        (nnBase, amin) = baseNN modLP nnFac
        nnAdd = additionalNN nnBase nnFac abd
        nnWfac = do
            nn <- nnAdd
            let nnRec = recursiveConnections nn (overlappingAtoms modLP abd)
            return $ NNwithAmin nnRec amin

lp2nn_no_abd input = case (decode input :: Maybe LPtoNNnoABD) of
    Nothing -> text $ pack "Problems with lp2nn_no_abd json"
    Just decodedInp -> do
        converted <- liftAndCatchIO nnWfac
        Web.Scotty.json converted
      where
        inpLP = lpjosnTOlp $ JsonHandling.lpnoABD decodedInp
        nnFac = factorsTOnnfactors $ factorsnoABD decodedInp
        (nnBase, amin) = baseNN inpLP nnFac
        nnAdd = additionalNN nnBase nnFac []
        nnWfac = do
            nn <- nnAdd
            let nnRec = recursiveConnections nn []
            return $ (NNwithFactors nnRec (factorsnoABD decodedInp), amin)

lp2nn_base input = case (decode input :: Maybe LPtoNNnoABD) of
    Nothing -> text $ pack "Problems with lp2nn_base json"
    Just decodedInp -> Web.Scotty.json converted
      where
        inpLP = lpjosnTOlp $ JsonHandling.lpnoABD decodedInp
        nnFac = factorsTOnnfactors $ factorsnoABD decodedInp
        (nnBase, amin) = baseNN inpLP nnFac
        nnRec = recursiveConnections nnBase []
        converted = (NNwithFactors nnRec (factorsnoABD decodedInp), amin)
