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
module TranslationTp
    ( baseNN
    , additionalConnectionsIO
    , mergeNNupd
    , recursiveConnections
    , showNNPython
    ) where

import Auxiliary
import NeuralNetworks as NN
import LogicPrograms
import Data.List (length, maximum, map, find, (\\), delete, foldl1)
import Data.Char 
import System.Random


-- | Types for $A_{min}$ and W values.
type Amin = Float
type W = Float


-- | The base for the value $A_min$. 
aminBase :: LP -> Int -> Float
aminBase lp maxBH = (fromIntegral (maxBH - 1) / fromIntegral (maxBH + 1))


-- | The base for the weight of the connections in a neural network for a given
-- logic program. 
wBase :: LP -> Amin -> NNfactors -> Int -> Int -> Float
wBase lp amin nnF maxBodies maxHeads = maximum [fstCondition, sndCondition]
    where
        fstCondition = (2 / b) * (((log $ 1 + amin) - (log $ 1 - amin)) / ((fromIntegral maxBodies) * (amin - 1) + amin + 1))
        sndCondition = (2 / b) * (((log $ 1 + amin) - (log $ 1 - amin) - (r * (fromIntegral $ l + 1)) ) / ((fromIntegral maxHeads) * (amin - 1) + amin + 1))
        b = beta nnF
        r = addWeightLimit nnF
        l = addHidNeuNumber nnF


-- | Base neural network for a given logic program. The truth neuron is added
-- regardless of presence of facts in the logic program.
baseNN :: LP -> NNfactors -> NeuralNetwork
baseNN lp nnF = mergeNNupd baseForNN (truthNN w)
    where
        -- list of all bodies length in the logic program
        bdsLen = bodiesLength lp
        
        -- list of numbers of clauses with the same head for every clause in the
        -- logic program
        clsSH  = clsSameHeads lp
        
        -- maximal number of atoms in bodies of clauses and clauses with the
        -- same heads
        maxBds = maximum $ bdsLen
        maxHds = maximum $ clsSH
        
        -- A_min and W values for the neural network
        amin   = aminBase lp (maximum [maxBds, maxHds]) + (aminFactor nnF)
        w      = wBase lp amin nnF maxBds maxHds + (weightFactor nnF)
        
        -- list of triples: clause, body length of the clause, number of clauses
        -- with the same head
        triLP  = zip3 lp bdsLen clsSH
        
        -- list of overlapping atoms in the logic program
        ovrl   = overlappingAtoms lp
        
        -- function that creates the basic neural network
        baseForNN = baseNNsteps triLP emptyNN amin w ovrl


-- | Base neural network created on the ground of list of triples that contain
-- clauses along with information about the body length and the number of
-- clauses with the same heads.
-- TODO: extend for assumptions
baseNNsteps :: [(Clause, Int, Int)] -> NeuralNetwork -> Amin -> W -> [OverlappingAtoms] -> NeuralNetwork
baseNNsteps [] nn amin w ovrl     = nn
baseNNsteps (t:ts) nn amin w ovrl = baseNNsteps ts newNN amin w ovrl
    where
        newNN = mergeNNupd nn (nnUpdFromTriple nn t amin w ovrl)


-- Creates update for a neural network given a triple: clause, its body length
-- and the number of clauses with the same head.
nnUpdFromTriple :: NeuralNetwork -> (Clause, Int, Int) -> Amin -> W -> [OverlappingAtoms] -> NNupdate
nnUpdFromTriple nn (cl, bdLen, sameHds) amin w ovrl = case cl of
    Fact _   -> updFromFact cl nn outBias ovrl w
    Cl _ _ _ -> updFromClause cl nn outBias hidBias w
    where
        outBias = w * (1 + amin) * (1 - fromIntegral sameHds) / 2
        hidBias = w * (1 + amin) * (fromIntegral bdLen - 1) / 2


-- | Update for a neural network in case of a fact.
updFromFact :: Clause -> NeuralNetwork -> Float -> [OverlappingAtoms] -> W -> NNupdate
updFromFact (Fact hd) nn outBias ovrl w = case outNeuOld of
    -- there is no output layer neuron associated with the head of the fact
    Nothing
        -- atom is with h index or it is already in the input layer;
        -- we add only output layer neuron and the connection to truth neuron
        | (elem hd ovrlH) || (not $ null inpNeuron) ->
            NNupdate 
                { inpNeuToAdd      = []
                , hidNeuToAdd      = []
                , outNeuToAdd      = [Neuron hdLabel "tanh" outBias outIdxLabel]
                , outNeuToRemove   = []
                , inpToHidConToAdd = []
                , hidToOutConToAdd = [Connection "hidT" outIdxLabel w]
                }
        -- otherwise, we additionally add an input layer neuron
        | otherwise ->
            NNupdate
                { inpNeuToAdd      = [Neuron hdLabel "idem" 0.0 inpIdxLabel]
                , hidNeuToAdd      = []
                , outNeuToAdd      = [Neuron hdLabel "tanh" outBias outIdxLabel]
                , outNeuToRemove   = []
                , inpToHidConToAdd = []
                , hidToOutConToAdd = [Connection "hidT" outIdxLabel w]
                }
    -- there is an output layer neuron associated with the head of the fact
    Just (Neuron _ _ outNeuOldBias outNeuOldLab)
        -- the bias of the output layer neuron is greater than the current one;
        -- we replace the old output layer neuron with a new one
        | outNeuOldBias > outBias ->
            NNupdate
                { inpNeuToAdd      = []
                , hidNeuToAdd      = []
                , outNeuToAdd      = [Neuron hdLabel "tanh" outBias outNeuOldLab]
                , outNeuToRemove   = [Neuron hdLabel "tanh" outNeuOldBias outNeuOldLab]
                , inpToHidConToAdd = []
                , hidToOutConToAdd = [Connection "hidT" outNeuOldLab w]
                }
        -- otherwise, we add the connection between the found neuron and the
        -- truth neuron
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
        ovrlH        = map snd ovrl


-- | Update for a neural network in case of a clause.
updFromClause :: Clause -> NeuralNetwork -> Float -> Float -> W -> NNupdate
updFromClause (Cl hd pBod nBod) nn outBias hidBias w = case outNeuOld of
    -- there is no output layer neuron associated with the head of the clause;
    -- we add input layer neurons, hidden layer neuron, output layer neurons and
    -- connections: from new input layer neurons to new hidden layer neuron and
    -- from new hidden layer neuron to new output layer neuron
    Nothing ->
        NNupdate
            { inpNeuToAdd      = inputNs
            , hidNeuToAdd      = [hidNeuron]
            , outNeuToAdd      = [Neuron hdLabel "tanh" outBias outNeuIdx] ++ outputNs 1
            , outNeuToRemove   = []
            , inpToHidConToAdd = inpToHidConns
            , hidToOutConToAdd = [Connection hidNeuIdx outNeuIdx w]
            }
    -- there is an output layer neuron associated with the head of the clause
    Just (Neuron _ _ outNeuOldBias outNeuOldIdx)
        -- the bias of the old output layer neuron is greater than the new one;
        -- we add input layer neurons, a hidden layer neuron, we replace the old
        -- output layer neuron with the new one and add other output layer
        -- neurons, we create connections between new input layer neurons and
        -- new hidden layer neuron, and between new hidden layer neuron and the
        -- replaced output layer neuron
        | outNeuOldBias > outBias ->
            NNupdate
                { inpNeuToAdd      = inputNs
                , hidNeuToAdd      = [hidNeuron]
                , outNeuToAdd      = [Neuron hdLabel "tanh" outBias outNeuOldIdx] ++ outputNs 0
                , outNeuToRemove   = [Neuron hdLabel "tanh" outNeuOldBias outNeuOldIdx]
                , inpToHidConToAdd = inpToHidConns
                , hidToOutConToAdd = [Connection hidNeuIdx outNeuOldIdx w]
                }
        -- otherwise, we add input layer neurons, new hidden layer neuron,
        -- output layer neurons (without the head of the clause), we create
        -- connections between new input layer neurons and new hidden layer
        -- neuron, and new hidden layer neuron and the old output layer neuron
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
        inputNs
            | (elem hd (pBod ++ nBod)) = createInpNeurons (pBod ++ nBod) ((+) 1 $ length $ inpLayer nn) (inpLayer nn)
            | otherwise                = createInpNeurons (hd : pBod ++ nBod) ((+) 1 $ length $ inpLayer nn) (inpLayer nn)
        outputNs x    = createOutNeurons (pBod ++ nBod) ((+) (1 + x) $ length $ outLayer nn) (outLayer nn)
        inpToHidConns = createInpToHidConn hidNeuIdx (inputNs ++ inpLayer nn) pBod nBod w
        

-- | Addition of neurons associated with given list of atoms to a given input
-- layer.
createInpNeurons :: [Atom] -> Int -> [Neuron] -> [Neuron]
createInpNeurons [] _ _                = []
createInpNeurons (a:as) idxStart inpNs = case findNeuByLabel a inpNs of
    Nothing -> mkInpNeu a : createInpNeurons as (idxStart + 1) inpNs
    Just _  -> createInpNeurons as idxStart inpNs
    where
        mkInpNeu x = Neuron (show x) "idem" 0.0 ("inp" ++ show idxStart)


-- | Addition of neurons associated with given list of atoms to a given output
-- layer.
createOutNeurons :: [Atom] -> Int -> [Neuron] -> [Neuron]
createOutNeurons [] _ _                = []
createOutNeurons (a:as) idxStart outNs = case findNeuByLabel a outNs of
    Nothing -> mkOutNeu a : createOutNeurons as (idxStart + 1) outNs
    Just _  -> createOutNeurons as idxStart outNs
    where
        mkOutNeu x = Neuron (show x) "tanh" 0.0 ("out" ++ show idxStart)


-- | Creates connections from given input layer neurons to a given hidden layer
-- neuron.
createInpToHidConn :: String -> [Neuron] -> [Atom] -> [Atom] -> W -> [Connection]
createInpToHidConn hidIdx inpNs pBod nBod w =
    [ Connection (NN.idx n) hidIdx w | n <- inpNs, any (\x -> show x == NN.label n) pBod ] ++
    [ Connection (NN.idx n) hidIdx (-w) | n <- inpNs, any (\x -> show x == NN.label n) nBod ]


-- | Finds neuron in a given list of neurons by a given label.
findNeuByLabel :: Atom -> [Neuron] -> Maybe Neuron
findNeuByLabel a ns = find (\x -> NN.label x == show a) ns


-- | Merges an update for neural network with a given neural network.
mergeNNupd :: NeuralNetwork -> NNupdate -> NeuralNetwork
mergeNNupd nn nnUpd = NN 
    { inpLayer            = inpLayer nn ++ inpNeuToAdd nnUpd
    , hidLayer            = hidLayer nn ++ hidNeuToAdd nnUpd
    , outLayer            = ((outLayer nn) \\ (outNeuToRemove nnUpd)) ++ outNeuToAdd nnUpd
    , recLayer            = recLayer nn
    , inpToHidConnections = inpToHidConnections nn ++ inpToHidConToAdd nnUpd
    , hidToOutConnections = hidToOutConnections nn ++ hidToOutConToAdd nnUpd
    , recConnections      = recConnections nn
    }


-- | Add recursive connections to a given neural network.
recursiveConnections :: NeuralNetwork -> [OverlappingAtoms] -> NeuralNetwork
recursiveConnections nn ovrl = NN
    { inpLayer            = inpLayer nn
    , hidLayer            = hidLayer nn
    , outLayer            = outLayer nn
    , recLayer            = recursiveNs
    , inpToHidConnections = inpToHidConnections nn
    , hidToOutConnections = hidToOutConnections nn
    , recConnections      = recursiveConns
    }
    where
        remJust (Just x ) = x
        tupleAToN (x, y)  = (remJust $ findNeuByLabel x (outLayer nn), remJust $ findNeuByLabel y (outLayer nn))
        ovrlNs            = map tupleAToN ovrl
        notOvrlNs         = [ n |
            n <- outLayer nn,
            not $ elem (NN.label n) (map show $ fst $ unzip ovrl),
            not $ elem (NN.label n) (map show $ snd $ unzip ovrl) ]
        regularRecConns   = createRecConnNormal (inpLayer nn) notOvrlNs
        abnormalRecConns  = fst $ createRecConnAbnormal (inpLayer nn) ovrlNs
        recursiveNs       = snd $ createRecConnAbnormal (inpLayer nn) ovrlNs
        recursiveConns    = regularRecConns ++ abnormalRecConns


-- | Creates recursive connections for non-overlapping atoms.
createRecConnNormal :: [Neuron] -> [Neuron] -> [Connection]
createRecConnNormal inpL outL =
    [ Connection (NN.idx outN) (NN.idx inpN) 1 |
        inpN <- inpL,
        outN <- outL,
        NN.label inpN == NN.label outN ]


-- | Creates recursive connections for overlapping atoms.
createRecConnAbnormal :: [Neuron] -> [(Neuron, Neuron)] -> ([Connection], [Neuron])
createRecConnAbnormal inpL ovrlN = foldl mergeTriCN ([], []) triplesCN
    where
        remJust (Just x)                 = x
        tri (x, y)                       = (remJust $ find (\z -> NN.label x == NN.label z) inpL, x, y)
        triplesN                         = map tri ovrlN
        triplesCN                        = map recConnFromTriple triplesN
        mergeTriCN (cs1, ns1) (cs2, ns2) = (cs1 ++ cs2, ns1 ++ ns2)


-- | Helper function for @createRecSonnAbnormal@ that adds connections for
-- a triple of neurons---it is used for "abnormal" recursive connections, where
-- it is necessary to add additional neuron.
recConnFromTriple :: (Neuron, Neuron, Neuron) -> ([Connection], [Neuron])
recConnFromTriple (inpN, outN1, outN2) = ([c1, c2, c3], [n])
    where
        n  = Neuron ("rec" ++ NN.label outN1) "k" 0.0 ("rec" ++ NN.label outN1)
        c1 = Connection (NN.idx outN1) (NN.idx n) 1
        c2 = Connection (NN.idx outN2) (NN.idx n) 1
        c3 = Connection (NN.idx n) (NN.idx inpN) 1


additionalConnections :: NeuralNetwork -> Int -> Float -> [Float] -> NeuralNetwork
additionalConnections nn l ba rs = case addConnsFromTriple of 
    (hidNeurons, inpToHidConns, hidToOutConns) -> NN
        { inpLayer            = inpLayer nn
        , hidLayer            = hidLayer nn ++ hidNeurons
        , outLayer            = outLayer nn
        , recLayer            = recLayer nn
        , inpToHidConnections = inpToHidConnections nn ++ inpToHidConns
        , hidToOutConnections = hidToOutConnections nn ++ hidToOutConns ++ hidTToOutConns nn (outLayer nn) rs
        , recConnections      = recConnections nn
        }
    where
        addConnsFromTriple = case unzip3 mkAddConns of (ns, xs, ys) -> (ns, concat xs, ys)
        mkAddConns         = makeAddConns nn (outLayer nn) l ba (drop outLayerLen rs) hidLayerLen
        hidLayerLen        = length $ hidLayer nn
        outLayerLen        = length $ outLayer nn


hidTToOutConns :: NeuralNetwork -> [Neuron] -> [Float] -> [Connection]
hidTToOutConns _ [] _      = []
hidTToOutConns nn (n:ns) (r:rs) = case findConnByNeu n (hidToOutConnections nn) of 
    Nothing -> Connection "hidT" (NN.idx n) r : hidTToOutConns nn ns rs
    Just _  -> hidTToOutConns nn ns (r:rs)


makeAddConns :: NeuralNetwork -> [Neuron] -> Int -> Float -> [Float] -> Int -> [(Neuron, [Connection], Connection)]
makeAddConns _ [] _ _ _ _              = []
makeAddConns nn (n:ns) l ba rs idxStart = case findConnByNeu n (hidToOutConnections nn) of 
    Nothing -> lNeu ++ makeAddConns nn ns l ba newrs (idxStart + l)
    Just _  -> makeAddConns nn ns l ba newrs idxStart
    where 
        lNeu   = createLNs nn n l ba rs idxStart
        newrs  = drop (l * (inpLen - 2) + 1) rs
        inpLen = length (inpLayer nn)


createLNs :: NeuralNetwork -> Neuron -> Int -> Float -> [Float] -> Int -> [(Neuron, [Connection], Connection)]
createLNs _ _ 0 _ _ _               = []
createLNs nn n l ba (r:rs) idxStart = (mkAddHidNeu, mkInpToHidConns, mkHidToOutConn) : createLNs nn n (l-1) ba newrs (idxStart + 1)
    where 
        mkAddHidNeu     = Neuron ("ha" ++ show idxStart) "tanh" ba ("hid" ++ show idxStart)
        mkInpToHidConns = case findInpOut nn n of
            Nothing     -> []
            Just inpNeu -> [ Connection (NN.idx inp) addHidNeuIdx w | (w, inp) <- zip rs (inpLayer nn \\ [inpNeu, inpT]) ]
        mkHidToOutConn  = Connection addHidNeuIdx (NN.idx n) r
        addHidNeuIdx    = NN.idx mkAddHidNeu
        inpT            = last $ inpLayer nn
        newrs           = drop (l * (inpLen-1)^2) rs
        inpLen          = length (inpLayer nn)

findConnByNeu :: Neuron -> [Connection] -> Maybe Connection 
findConnByNeu outNeu hidToOutConns = find (\x -> from x == "hidT" && NN.idx outNeu == to x) hidToOutConns


findInpOut :: NeuralNetwork -> Neuron -> Maybe Neuron
findInpOut nn n = find (\x -> NN.label x == NN.label n) (inpLayer nn) 


rand :: Float -> IO [Float]
rand r = newStdGen >>= return . randomRs (-r, r)


additionalConnectionsIO :: NeuralNetwork -> Int -> Float -> Float -> IO NeuralNetwork
additionalConnectionsIO nn l ba r = do 
    rs <- rand r
    return (additionalConnections nn l ba rs)


showNNPython :: NeuralNetwork -> IO String
showNNPython x = do
    nn <- additionalConnectionsIO x 1 0.0 0.05
    return $ nnToPythonString nn
