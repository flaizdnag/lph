{-|
Module      : Translation
Description : Functions that allow to translate a logic program into a neural
              network.
Copyright   : (c) Aleksandra Cz., 2019
                  Kinga O., 2019
                  Andrzej G., 2019
License     : GPL-3length
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module Translation
    ( ) where

import Auxiliary
import NeuralNetworks
import LogicPrograms
import Data.List (length, maximum, map, find, (\\), delete, partition)


data NNupdate = NNupdate
    { inpNeuToAdd      :: [Neuron]
    , hidNeuToAdd      :: [Neuron]
    , outNeuToAdd      :: [Neuron]
    , outNeuToRemove   :: [Neuron]
    , inpToHidConToAdd :: [Connection]
    , hidToOutConToAdd :: [Connection]
    }
    deriving (Show, Read)


-- | Function that returns length of the body of a given Horn clause.
bodyLength :: Clause -> Int 
bodyLength = length . clBody


-- | Function that returns lengths of all bodies of Horn clauses from a given
-- logic program. 
bodiesLength :: LP -> [Int] 
bodiesLength = map bodyLength


-- | Function that returns the number of Horn clauses that have the same atom
-- in their head as the given Horn clause.
clSameHeads :: Clause -> LP -> Int 
clSameHeads cl lp = length [ cls | cls <- lp, clHead cls == clHead cl ]


-- | Function that returns the number of Horn clauses that have the same atom
-- in their head as the given Horn clause for every Horn clause in a given 
-- logic program. 
clsSameHeads :: LP -> [Int]
clsSameHeads lp = map (\x -> clSameHeads x lp) lp


-- | Function that returns the base for the value A_min. 
aminBase :: LP -> Int -> Float
aminBase lp maxBH = (fromIntegral (maxBH - 1) / fromIntegral (maxBH + 1))


-- | Function that returns weight of the connections in neural network for 
-- a given logic program. 
wBase :: LP -> Float -> Float -> Int -> Float -> Int -> Int -> Float
wBase lp amin r l beta maxBodies maxHeads = maximum [fstCondition, sndCondition]
    where
        fstCondition = (2 / beta) * (((log $ 1 + amin) - (log $ 1 - amin)) / ((fromIntegral maxBodies) * (amin - 1) + amin + 1))
        sndCondition = (2 / beta) * (((log $ 1 + amin) - (log $ 1 - amin) - (r * (fromIntegral $ l + 1)) ) / ((fromIntegral maxHeads) * (amin - 1) + amin + 1))


overlappingAtoms :: LP -> [Atom]
overlappingAtoms lp = [ atom |
    atom <- bp lp,
    elem 'h' (LogicPrograms.label atom),
    any (\x -> LogicPrograms.idx atom == LogicPrograms.idx x && eqLists (delete 'h' (LogicPrograms.label atom)) (LogicPrograms.label x)) (bp lp) ]


baseNN :: LP -> Float -> Float -> Float -> Float -> Float -> Int -> NeuralNetwork
baseNN lp aminF wF beta addBias r l = mergeNNupd (baseNNsteps triLP emptyNN amin w ovrl) (truthNN w)
    where
        bdsLen = bodiesLength lp
        clsSH  = clsSameHeads lp
        maxBds = maximum $ bdsLen
        maxHds = maximum $ clsSH
        amin   = aminBase lp (maximum [maxBds, maxHds]) + aminF
        w      = wBase lp amin r l beta maxBds maxHds + wF
        triLP  = zip3 lp bdsLen clsSH
        ovrl   = overlappingAtoms lp


truthNN :: Float -> NNupdate
truthNN w = NNupdate
    { inpNeuToAdd      = [Neuron "inpT" "const" 0.0 "inpT"]
    , hidNeuToAdd      = [Neuron "hidT" "tanh" 0.0 "hidT"]
    , outNeuToAdd      = []
    , outNeuToRemove   = []
    , inpToHidConToAdd = [Connection "inpT" "hidT" w]
    , hidToOutConToAdd = []
    }


baseNNsteps :: [(Clause, Int, Int)] -> NeuralNetwork -> Float -> Float -> [Atom] -> NeuralNetwork
baseNNsteps [] nn amin w ovrl     = nn
baseNNsteps (t:ts) nn amin w ovrl = baseNNsteps ts newNN amin w ovrl
    where
        newNN = mergeNNupd nn (nnUpdFromTriple nn t amin w ovrl)


nnUpdFromTriple :: NeuralNetwork -> (Clause, Int, Int) -> Float -> Float -> [Atom] -> NNupdate
nnUpdFromTriple nn (cl, bdLen, sameHds) amin w ovrl = case cl of
    Fact _   -> updFromFact cl nn outBias ovrl w
    Cl _ _ _ -> updFromClause cl nn outBias hidBias ovrl w
    where
        outBias = w * (1 + amin) * (1 - fromIntegral sameHds) / 2
        hidBias = w * (1 + amin) * (fromIntegral bdLen - 1) / 2


updFromFact :: Clause -> NeuralNetwork -> Float -> [Atom] -> Float -> NNupdate
updFromFact (Fact hd) nn outBias ovrl w = case outNeuOld of
    Nothing
        | (elem hd ovrl) || (not $ null inpNeuron) ->
            NNupdate 
                { inpNeuToAdd      = []
                , hidNeuToAdd      = []
                , outNeuToAdd      = [Neuron hdLabel "tanh" outBias outIdxLabel]
                , outNeuToRemove   = []
                , inpToHidConToAdd = []
                , hidToOutConToAdd = [Connection "hidT" outIdxLabel w]
                }
        | otherwise ->
            NNupdate
                { inpNeuToAdd      = [Neuron hdLabel "idem" 0.0 inpIdxLabel]
                , hidNeuToAdd      = []
                , outNeuToAdd      = [Neuron hdLabel "tanh" outBias outIdxLabel]
                , outNeuToRemove   = []
                , inpToHidConToAdd = []
                , hidToOutConToAdd = [Connection "hidT" outIdxLabel w]
                }
    Just (Neuron _ _ outNeuOldBias outNeuOldLab)
        | outNeuOldBias > outBias ->
            NNupdate
                { inpNeuToAdd      = []
                , hidNeuToAdd      = []
                , outNeuToAdd      = [Neuron hdLabel "tanh" outBias outNeuOldLab]
                , outNeuToRemove   = [Neuron hdLabel "tanh" outNeuOldBias outNeuOldLab]
                , inpToHidConToAdd = []
                , hidToOutConToAdd = [Connection "hidT" outNeuOldLab w]
                }
        | otherwise ->
            NNupdate
                { inpNeuToAdd      = []
                , hidNeuToAdd      = []
                , outNeuToAdd      = []
                , outNeuToRemove   = []
                , inpToHidConToAdd = []
                , hidToOutConToAdd = [Connection "hidT" outNeuOldLab w]
                }
    where
        outNeuOld    = findNeuByLabel hd (outLayer nn)
        inpNeuron    = findNeuByLabel hd (inpLayer nn)
        hdLabel      = show $ hd
        inpIdxLabel  = "inp" ++ (show $ (+) 1 $ length $ inpLayer nn)
        outIdxLabel  = "out" ++ (show $ (+) 1 $ length $ outLayer nn)


updFromClause :: Clause -> NeuralNetwork -> Float -> Float -> [Atom] -> Float -> NNupdate
updFromClause (Cl hd pBod nBod) nn outBias hidBias ovrl w = case outNeuOld of
    Nothing ->
        NNupdate
            { inpNeuToAdd      = inputNs
            , hidNeuToAdd      = [hidNeuron]
            , outNeuToAdd      = [Neuron hdLabel "tanh" outBias outNeuIdx] ++ outputNs 1
            , outNeuToRemove   = []
            , inpToHidConToAdd = inpToHidConns
            , hidToOutConToAdd = [Connection hidNeuIdx outNeuIdx w]
            }
    Just (Neuron _ _ outNeuOldBias outNeuOldIdx)
        | outNeuOldBias > outBias ->
            NNupdate
                { inpNeuToAdd      = inputNs
                , hidNeuToAdd      = [hidNeuron]
                , outNeuToAdd      = [Neuron hdLabel "tanh" outBias outNeuOldIdx] ++ outputNs 0
                , outNeuToRemove   = [Neuron hdLabel "tanh" outNeuOldBias outNeuOldIdx]
                , inpToHidConToAdd = inpToHidConns
                , hidToOutConToAdd = [Connection hidNeuIdx outNeuOldIdx w]
                }
        | otherwise ->
            NNupdate
                { inpNeuToAdd      = inputNs
                , hidNeuToAdd      = [hidNeuron]
                , outNeuToAdd      = outputNs 0
                , outNeuToRemove   = []
                , inpToHidConToAdd = inpToHidConns
                , hidToOutConToAdd = [Connection hidNeuIdx outNeuOldIdx w]
                }
    where
        hidNeuIdxNum  = show $ (+) 1 $ length $ hidLayer nn
        hidNeuIdx     = "hid" ++ hidNeuIdxNum
        hidNeuron     = Neuron ("h" ++ hidNeuIdxNum) "tanh" hidBias hidNeuIdx
        outNeuOld     = findNeuByLabel hd (outLayer nn)
        hdLabel       = show $ hd
        outNeuIdx     = "out" ++ (show $ (+) 1 $ length $ outLayer nn)
        inputNs       =
            if (elem hd (pBod ++ nBod)) then
                createInpNeurons (pBod ++ nBod) ((+) 1 $ length $ inpLayer nn) (inpLayer nn)
            else
                createInpNeurons (hd : pBod ++ nBod) ((+) 1 $ length $ inpLayer nn) (inpLayer nn)
        outputNs      = \x -> createOutNeurons (pBod ++ nBod) ((+) (1 + x) $ length $ outLayer nn) (outLayer nn)
        inpToHidConns = createInpToHidConn hidNeuIdx inputNs pBod nBod w


createInpNeurons :: [Atom] -> Int -> [Neuron] -> [Neuron]
createInpNeurons [] _ _                = []
createInpNeurons (a:as) idxStart inpNs = case findNeuByLabel a inpNs of
    Nothing -> mkInpNeu a : createInpNeurons as (idxStart + 1) inpNs
    Just _  -> createInpNeurons as idxStart inpNs
    where
        mkInpNeu = \x -> Neuron (show x) "idem" 0.0 ("inp" ++ show idxStart)


createOutNeurons :: [Atom] -> Int -> [Neuron] -> [Neuron]
createOutNeurons [] _ _        = []
createOutNeurons (a:as) idxStart outNs = case findNeuByLabel a outNs of
    Nothing -> mkInpNeu a : createOutNeurons as (idxStart + 1) outNs
    Just _  -> createOutNeurons as idxStart outNs
    where
        mkInpNeu = \x -> Neuron (show x) "tanh" 0.0 ("out" ++ show idxStart)


createInpToHidConn :: String -> [Neuron] -> [Atom] -> [Atom] -> Float -> [Connection]
createInpToHidConn hidIdx inpNs pBod nBod w =
    [ Connection (NeuralNetworks.idx n) hidIdx w | n <- inpNs, any (\x -> show x == NeuralNetworks.label n) pBod ] ++
    [ Connection (NeuralNetworks.idx n) hidIdx (-w) | n <- inpNs, any (\x -> show x == NeuralNetworks.label n) nBod ]


findNeuByLabel :: Atom -> [Neuron] -> Maybe Neuron
findNeuByLabel a ns = find (\x -> NeuralNetworks.label x == show a) ns


mergeNNupd :: NeuralNetwork -> NNupdate -> NeuralNetwork
mergeNNupd nn nnUpd = NN 
    { inpLayer            = inpLayer nn ++ inpNeuToAdd nnUpd
    , hidLayer            = hidLayer nn ++ hidNeuToAdd nnUpd
    , outLayer            = ((outLayer nn) \\ (outNeuToRemove nnUpd)) ++ outNeuToAdd nnUpd
    , recLayer            = recLayer nn
    , inpToHidConnections = inpToHidConnections nn ++ inpToHidConToAdd nnUpd
    , hidToOutConnections = hidToOutConnections nn ++ hidToOutConToAdd nnUpd
    , recConnections      = recConnections nn
    , addConnections      = addConnections nn
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
    , addConnections      = []
    }


recursiveConnections :: NeuralNetwork -> [Atom] -> NeuralNetwork
recursiveConnections nn ovrl = emptyNN


--createRecursiveConns :: [Neuron] -> [Neuron] -> [Atom] -> NNupdate
createRecursiveConns inpLayer outLayer ovrl = divided
    where
        divided = partition condition outLayer
        condition = \x -> elem (NeuralNetworks.label x) (map show ovrl) || any (\y -> eqLists y (NeuralNetworks.label x ++ "^h")) (map show ovrl)


--createRecConn :: Neuron -> [Neuron] -> [Atom] 


p1 :: LP
p1 = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 1 "") [A 3 ""] [], Fact (A 5 "")]

p1NN :: NeuralNetwork
p1NN = baseNN p1 0.5 0.5 1 0.0 0.05 2 
