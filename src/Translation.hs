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

import NeuralNetworks
import LogicPrograms
import Data.List (length, maximum, map, find, (\\))


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
    any (\x -> LogicPrograms.idx atom == LogicPrograms.idx x && notElem 'h' (LogicPrograms.label x)) (bp lp) ]


baseNN :: LP -> Float -> Float -> Float -> Float -> Float -> Int -> NeuralNetwork
baseNN lp aminF wF beta addBias r l = baseNNsteps triLP emptyNN amin w ovrl
    where
        bdsLen = bodiesLength lp
        clsSH  = clsSameHeads lp
        maxBds = maximum $ bdsLen
        maxHds = maximum $ clsSH
        amin   = aminBase lp (maximum [maxBds, maxHds]) + aminF
        w      = wBase lp amin r l beta maxBds maxHds + wF
        triLP  = zip3 lp bdsLen clsSH
        ovrl   = overlappingAtoms lp


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
                { inpNeuToAdd      = [Neuron hdLabel "const" 0.0 inpIdxLabel]
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
        mkInpNeu = \x -> Neuron (show x) "const" 0.0 ("inp" ++ show idxStart)


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



{-
addNeurons :: [Atom] -> NeuralNetwork -> Float -> Float -> String -> NeuralNetwork
addNeurons [] nn hidBias outBias inf = nn
addNeurons (a:as) nn hidBias outBias inf = addNeurons as newNN hidBias outBias inf
    where
        newNN = addNeuron a nn hidBias outBias inf

addNeuron :: Atom -> NeuralNetwork -> Float -> Float -> String -> NeuralNetwork
addNeuron a nn hidBias outBias inf = case inf of
    "inp"
        | isThereNeuron (inpLayer nn) -> nn
        | otherwise                   -> NN
            { inpLayer            = newInpNeuron : inpLayer nn
            , hidLayer            = hidLayer nn
            , outLayer            = outLayer nn
            , recLayer            = recLayer nn
            , inpToHidConnections = inpToHidConnections nn
            , hidToOutConnections = hidToOutConnections nn
            , recConnections      = recConnections nn
            , addConnections      = addConnections nn}
    "hid"
        | isThereNeuron (hidLayer nn) -> nn
        | otherwise                   -> NN
            { inpLayer            = inpLayer nn
            , hidLayer            = newHidNeuron : hidLayer nn
            , outLayer            = outLayer nn
            , recLayer            = recLayer nn
            , inpToHidConnections = inpToHidConnections nn
            , hidToOutConnections = hidToOutConnections nn
            , recConnections      = recConnections nn
            , addConnections      = addConnections nn}
    "out"
        | isThereNeuron (outLayer nn) -> nn
        | otherwise                   -> NN
            { inpLayer            = inpLayer nn
            , hidLayer            = hidLayer nn
            , outLayer            = newOutNeuron : outLayer nn
            , recLayer            = recLayer nn
            , inpToHidConnections = inpToHidConnections nn
            , hidToOutConnections = hidToOutConnections nn
            , recConnections      = recConnections nn
            , addConnections      = addConnections nn}
    where
        isThereNeuron = \nl -> any (\x -> NeuralNetworks.label x == show a) nl
        newInpNeuron = Neuron
            { NeuralNetworks.label = show a
            , activFunc            = "identity"
            , bias                 = 0.0
            , NeuralNetworks.idx   = "inp" ++ show (length (inpLayer nn) + 1)}
        newHidNeuron = Neuron
            { NeuralNetworks.label = show a
            , activFunc            = "tanh"
            , bias                 = hidBias
            , NeuralNetworks.idx   = "hid" ++ show (length (hidLayer nn) + 1)}
        newOutNeuron = Neuron
            { NeuralNetworks.label = show a
            , activFunc            = "tanh"
            , bias                 = outBias
            , NeuralNetworks.idx   = "out" ++ show (length (outLayer nn) + 1)}


-- | Function that returns a bias of a hidden layer neuron for a given Horn
-- clause and logic program.
biasHidN :: Clause -> LP -> Float
biasHidN cl lp = (((1 + amin lp) * (fromIntegral (bodyLength cl) - 1)) / 2) * w lp

-- | Function that returns a bias of an output layer neuron for a given Horn
-- clause and logic program.
biasOutN :: Clause -> LP -> Float
biasOutN cl lp = (((1 + amin lp) * (1 - fromIntegral (sameHeadsCl cl lp))) / 2) * w lp

-- | Function that converts an atom into an input layer neuron. 
makeInpN :: Atom -> Int -> Neuron
makeInpN (A idx lab) n = Neuron {Translation.label = "a" ++ show idx, actF = 1, bias = 0, Translation.idx = "I" ++ show n}

-- | Function that converts an atom into an output layer neuron. 
makeOutN :: Atom -> Int -> Float -> Neuron
makeOutN (A idx lab) n b = Neuron {Translation.label = "a" ++ show idx, actF = 2, bias = b, Translation.idx = "O" ++ show n}

-- | Function that returns a list of the input layer neurons in the neural 
-- network for a given logic program. 
inputNeurons :: LP -> [Neuron]
inputNeurons lp = zipWith makeInpN (bp lp) [1..]

-- | Function that returns a list of the output layer neurons in the neural
-- network for a given logic program. 
outputNeurons :: LP -> [Neuron]
outputNeurons lp = zipWith3 makeOutN (bp lp) [1..] bs
        where 
            bs = [ biasOutN cl lp | cl <- lp ] ++ [ 0 | _ <- onlyBodies lp ]
-}

p1 :: LP
p1 = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 1 "") [A 3 ""] [], Fact (A 5 "")]
