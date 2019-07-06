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
import TranslationTp


-- | NNupdate that contains Top and Bottom neurons for the input layer.
topBotNN :: NNupdate
topBotNN = NNupdate
    { inpNeuToAdd      = [Neuron "Top" "-0.5" 0.0 "inpTop", Neuron "Bot" "-0.5" 0.0 "inpBot"]
    , hidNeuToAdd      = []
    , outNeuToAdd      = []
    , outNeuToRemove   = []
    , inpToHidConToAdd = []
    , hidToOutConToAdd = []
    }


findNeuByLabel :: Atom -> [Neuron] -> Maybe Neuron
findNeuByLabel a ns = find (\x -> NeuralNetworks.label x == (show a) ++ "Top") ns


baseNN :: LP -> Float -> NeuralNetwork
baseNN lp w = mergeNNupd (baseNNsteps triLP emptyNN w) topBotNN
    where
        bdsLen = bodiesLength lp
        clsSH  = clsSameHeads lp
        triLP  = zip3 lp bdsLen clsSH


baseNNsteps :: [(Clause, Int, Int)] -> NeuralNetwork -> Float -> NeuralNetwork
baseNNsteps [] nn w     = nn
baseNNsteps (t:ts) nn w = baseNNsteps ts newNN w 
    where
        newNN = mergeNNupd nn (nnUpdFromTriple t nn w)


nnUpdFromTriple :: (Clause, Int, Int) -> NeuralNetwork -> Float -> NNupdate
nnUpdFromTriple (cl, bdLen, sameHds) nn w = case cl of 
    Assumption _ -> updFromAssumption cl nn w biasHidAF biasOut
    Fact _       -> updFromFact cl nn w biasHidAF biasOut
    Cl _ _ _     -> updFromClause cl nn w biasHidClTop biasHidClBot biasOut
    where 
        biasHidAF    = w / 2
        biasHidClTop = (fromIntegral bdLen * w) - (w / 2)
        biasHidClBot = w / 2 
        biasOut      = maximum (w / 2, (fromIntegral sameHds * w) - w / 2)


updFromAssumption :: Clause -> NeuralNetwork -> Float -> Float -> Float -> NNupdate
updFromAssumption (Assumption hd) nn w biasHid biasOut = case outNeuOld of 
    Nothing -> 
        NNupdate 
            { inpNeuToAdd      = []
            , hidNeuToAdd      = hidNs
            , outNeuToAdd      = [Neuron hdLabelTop "threshold" biasOut outIdxLabelTop, 
                                  Neuron hdLabelBot "threshold" biasOut outIdxLabelBot]
            , outNeuToRemove   = []
            , inpToHidConToAdd = [Connection "Bot" hidNeuIdxBot w]
            , hidToOutConToAdd = [Connection hidNeuIdxTop outIdxLabelTop w, 
                                  Connection hidNeuIdxBot outIdxLabelBot w]
            }
    Just neu -> case neu of 
        (Neuron lab aF b outNeuIdx) ->
            NNupdate 
                { inpNeuToAdd      = []
                , hidNeuToAdd      = hidNs
                , outNeuToAdd      = []
                , outNeuToRemove   = []
                , inpToHidConToAdd = [Connection "Bot" hidNeuIdxBot w]
                , hidToOutConToAdd = [Connection hidNeuIdxTop outNeuIdx w, 
                                      Connection hidNeuIdxBot (NeuralNetworks.idx (remJust (lookup neu $ (zip <*> tail) (outLayer nn)))) w]
                }
    where
        outNeuOld       = findNeuByLabel hd (outLayer nn)
        inpNeuron       = findNeuByLabel hd (inpLayer nn)
        hdLabelTop      = (show $ hd) ++ "Top"
        hdLabelBot      = (show $ hd) ++ "Bot"
        inpIdxLabelTop  = "inp" ++ (show $ (+) 1 $ length $ inpLayer nn)
        inpIdxLabelBot  = "inp" ++ (show $ (+) 2 $ length $ inpLayer nn)
        outIdxLabelTop  = "out" ++ (show $ round (((fromIntegral $ length $ outLayer nn)/2) + 1))
        outIdxLabelBot  = "out" ++ (show $ round (((fromIntegral $ length $ outLayer nn)/2) + 2))
        hidNeuIdxNum    = show $ round (((fromIntegral $ length $ hidLayer nn)/2) + 1)
        hidNeuIdxNumTop = show $ (+) 1 $ length $ hidLayer nn
        hidNeuIdxNumBot = show $ (+) 2 $ length $ hidLayer nn
        hidNeuIdxTop    = "hid" ++ hidNeuIdxNumTop
        hidNeuIdxBot    = "hid" ++ hidNeuIdxNumBot
        hidNs           = [Neuron ("h" ++ hidNeuIdxNum ++ "Top") "threshold" biasHid hidNeuIdxTop, 
                           Neuron ("h" ++ hidNeuIdxNum ++ "Bot") "threshold" biasHid hidNeuIdxBot]
        remJust = \(Just x) -> x


updFromFact :: Clause -> NeuralNetwork -> Float -> Float -> Float -> NNupdate
updFromFact (Fact hd) nn w biasHid biasOut = case outNeuOld of 
    Nothing -> 
        NNupdate 
                { inpNeuToAdd      = []
                , hidNeuToAdd      = hidNs
                , outNeuToAdd      = [Neuron hdLabelTop "threshold" biasOut outIdxLabelTop, 
                                      Neuron hdLabelBot "threshold" biasOut outIdxLabelBot]
                , outNeuToRemove   = []
                , inpToHidConToAdd = [Connection "Top" hidNeuIdxTop w]
                , hidToOutConToAdd = [Connection hidNeuIdxTop outIdxLabelTop w, 
                                      Connection hidNeuIdxBot outIdxLabelBot w]
                }
    Just neu -> case neu of 
        Neuron lab aF b outNeuIdx ->
            NNupdate 
                { inpNeuToAdd      = []
                , hidNeuToAdd      = hidNs
                , outNeuToAdd      = []
                , outNeuToRemove   = []
                , inpToHidConToAdd = [Connection "Top" hidNeuIdxTop w]
                , hidToOutConToAdd = [Connection hidNeuIdxTop outNeuIdx w, 
                                      Connection hidNeuIdxBot (NeuralNetworks.idx (remJust (lookup neu $ (zip <*> tail) (outLayer nn)))) w]
                }
    where
        outNeuOld       = findNeuByLabel hd (outLayer nn)
        inpNeuron       = findNeuByLabel hd (inpLayer nn)
        hdLabelTop      = (show $ hd) ++ "Top"
        hdLabelBot      = (show $ hd) ++ "Bot"
        inpIdxLabelTop  = "inp" ++ (show $ (+) 1 $ length $ inpLayer nn)
        inpIdxLabelBot  = "inp" ++ (show $ (+) 2 $ length $ inpLayer nn)
        outIdxLabelTop  = "out" ++ (show $ round (((fromIntegral $ length $ outLayer nn)/2) + 1))
        outIdxLabelBot  = "out" ++ (show $ round (((fromIntegral $ length $ outLayer nn)/2) + 2))
        hidNeuIdxNum    = show $ round (((fromIntegral $ length $ hidLayer nn)/2) + 1)
        hidNeuIdxNumTop = show $ (+) 1 $ length $ hidLayer nn
        hidNeuIdxNumBot = show $ (+) 2 $ length $ hidLayer nn
        hidNeuIdxTop    = "hid" ++ hidNeuIdxNumTop
        hidNeuIdxBot    = "hid" ++ hidNeuIdxNumBot
        hidNs           = [Neuron ("h" ++ hidNeuIdxNum ++ "Top") "threshold" biasHid hidNeuIdxTop, 
                           Neuron ("h" ++ hidNeuIdxNum ++ "Bot") "threshold" biasHid hidNeuIdxBot]
        remJust = \(Just x) -> x

                
updFromClause :: Clause -> NeuralNetwork -> Float -> Float -> Float -> Float -> NNupdate
updFromClause (Cl hd pBod nBod) nn w biasHidTop biasHidBot biasOut = case outNeuOld of 
    Nothing -> 
        NNupdate
            { inpNeuToAdd      = inputNs
            , hidNeuToAdd      = hidNs
            , outNeuToAdd      = [Neuron hdLabelTop "threshold" biasOut outIdxLabelTop, 
                                  Neuron hdLabelBot "threshold" biasOut outIdxLabelBot] ++ outputNs 2
            , outNeuToRemove   = []
            , inpToHidConToAdd = inpToHidConnsTop ++ inpToHidConnsBot
            , hidToOutConToAdd = [Connection hidNeuIdxTop outIdxLabelTop w, 
                                  Connection hidNeuIdxBot outIdxLabelBot w]
            }
    Just neu -> case neu of 
        Neuron lab aF b outNeuIdx ->
            NNupdate
                { inpNeuToAdd      = inputNs
                , hidNeuToAdd      = hidNs
                , outNeuToAdd      = outputNs 0
                , outNeuToRemove   = []
                , inpToHidConToAdd = inpToHidConnsTop ++ inpToHidConnsBot
                , hidToOutConToAdd = [Connection hidNeuIdxTop outNeuIdx w, 
                                      Connection hidNeuIdxBot (NeuralNetworks.idx (remJust (lookup neu $ (zip <*> tail) (outLayer nn)))) w]
                }
    where 
        outNeuOld        = findNeuByLabel hd (outLayer nn)
        hdLabelTop       = (show $ hd) ++ "Top"
        hdLabelBot       = (show $ hd) ++ "Bot"
        outIdxLabelTop   = "out" ++ (show $ round (((fromIntegral $ length $ outLayer nn)/2) + 1))
        outIdxLabelBot   = "out" ++ (show $ round (((fromIntegral $ length $ outLayer nn)/2) + 2))
        inputNs          =
            if (elem hd (pBod ++ nBod)) then
                createInpNeurons (pBod ++ nBod) ((+) 1 $ length $ inpLayer nn) (inpLayer nn)
            else
                createInpNeurons (hd : pBod ++ nBod) ((+) 1 $ length $ inpLayer nn) (inpLayer nn)
        outputNs         = \x -> createOutNeurons (pBod ++ nBod) ((+) (1 + x) $ length $ outLayer nn) (outLayer nn) biasOut
        inpToHidConnsTop = createInpToHidConnTop hidNeuIdxTop (inputNs ++ inpLayer nn) pBod nBod w
        inpToHidConnsBot = createInpToHidConnBot hidNeuIdxBot (inputNs ++ inpLayer nn) pBod nBod w
        hidNeuIdxNum     = show $ round (((fromIntegral $ length $ hidLayer nn)/2) + 1)
        hidNeuIdxNumTop  = show $ (+) 1 $ length $ hidLayer nn
        hidNeuIdxNumBot  = show $ (+) 2 $ length $ hidLayer nn
        hidNeuIdxTop     = "hid" ++ hidNeuIdxNumTop
        hidNeuIdxBot     = "hid" ++ hidNeuIdxNumBot
        hidNs            = [Neuron ("h" ++ hidNeuIdxNum ++ "Top") "threshold" biasHidTop hidNeuIdxTop, 
                            Neuron ("h" ++ hidNeuIdxNum ++ "Bot") "threshold" biasHidBot hidNeuIdxBot]
        remJust = \(Just x) -> x


createInpNeurons :: [Atom] -> Int -> [Neuron] -> [Neuron]
createInpNeurons [] _ _                = []
createInpNeurons (a:as) idxStart inpNs = case findNeuByLabel a inpNs of
    Nothing -> mkInpNeuTop a : mkInpNeuBot a : createInpNeurons as (idxStart + 2) inpNs
    Just _  -> createInpNeurons as idxStart inpNs
    where
        mkInpNeuTop = \x -> Neuron (show x ++ "Top") "threshold" 0.5 ("inp" ++ show idxStart)
        mkInpNeuBot = \x -> Neuron (show x ++ "Bot") "threshold" 0.5 ("inp" ++ show (idxStart + 1))


createOutNeurons :: [Atom] -> Int -> [Neuron] -> Float -> [Neuron]
createOutNeurons [] _ _ _                       = []
createOutNeurons (a:as) idxStart outNs biasOut = case findNeuByLabel a outNs of
    Nothing -> mkOutNeuTop a : mkOutNeuBot a : createOutNeurons as (idxStart + 2) outNs biasOut
    Just _  -> createOutNeurons as idxStart outNs biasOut
    where
        mkOutNeuTop = \x -> Neuron (show x ++ "Top") "threshold" biasOut ("out" ++ show idxStart)
        mkOutNeuBot = \x -> Neuron (show x ++ "Bot") "threshold" biasOut ("out" ++ show (idxStart + 1))


createInpToHidConnTop :: String -> [Neuron] -> [Atom] -> [Atom] -> Float -> [Connection]
createInpToHidConnTop hidIdx inpNs pBod nBod w = 
    [ Connection (NeuralNetworks.idx n) hidIdx w | n <- inpNs, any (\x -> (show x ++ "Top") == NeuralNetworks.label n) pBod ] ++
    [ Connection (NeuralNetworks.idx n) hidIdx w | n <- inpNs, any (\x -> (show x ++ "Bot") == NeuralNetworks.label n) nBod ] 


createInpToHidConnBot :: String -> [Neuron] -> [Atom] -> [Atom] -> Float -> [Connection]
createInpToHidConnBot hidIdx inpNs pBod nBod w = 
    [ Connection (NeuralNetworks.idx n) hidIdx w | n <- inpNs, any (\x -> (show x ++ "Bot") == NeuralNetworks.label n) pBod ] ++
    [ Connection (NeuralNetworks.idx n) hidIdx w | n <- inpNs, any (\x -> (show x ++ "Top") == NeuralNetworks.label n) nBod ]

{-
p1 :: LP
p1 = [Cl (A 1 "") [A 2 ""] [A 3 ""], Assumption (A 3 "")]

p1NN :: NeuralNetwork
p1NN = baseNN p1 1

p1NNrec :: NeuralNetwork
p1NNrec = recursiveConnections p1NN []

p2 :: LP
p2 = [Cl (A 1 "") [A 2 ""] [A 3 ""], Assumption (A 3 ""), Cl (A 1 "") [A 4 ""] [A 5 ""], Assumption (A 5 "")]

p2NN :: NeuralNetwork
p2NN = baseNN p2 1

p2NNrec :: NeuralNetwork
p2NNrec = recursiveConnections p2NN []

p5 :: LP
p5 = [Cl (A 1 "") [A 2 ""] [A 3 ""], Assumption (A 3 ""), Fact (A 2 "")]

p5NN :: NeuralNetwork
p5NN = baseNN p5 1

p5NNrec :: NeuralNetwork
p5NNrec = recursiveConnections p5NN []
-}
