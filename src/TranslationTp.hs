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
    ( NNupdate(..) 
    , bodyLength
    , bodiesLength
    , clSameHeads
    , clsSameHeads
    , overlappingAtoms
    , baseNN
    , additionalConnectionsIO
    , emptyNN
    , emptyNNupd
    , mergeNNupd
    , recursiveConnections
    , showNNPython
    ) where

import Auxiliary
import NeuralNetworks
import LogicPrograms
import Data.List (length, maximum, map, find, (\\), delete, partition, foldl1)
import Data.Char 
import System.Random


-- | A special type for neural networks updates handling.
data NNupdate = NNupdate
    { inpNeuToAdd      :: [Neuron]
    , hidNeuToAdd      :: [Neuron]
    , outNeuToAdd      :: [Neuron]
    , outNeuToRemove   :: [Neuron]
    , inpToHidConToAdd :: [Connection]
    , hidToOutConToAdd :: [Connection]
    }
    deriving (Show, Read)


-- | The length of the body of a given clause.
bodyLength :: Clause -> Int 
bodyLength = length . clBody


-- | Lengths of all bodies of clauses from a given logic program. 
bodiesLength :: LP -> [Int] 
bodiesLength = map bodyLength


-- | The number of clauses that have the same atom in their head as the given
-- clause.
clSameHeads :: Clause -> LP -> Int 
clSameHeads cl lp = length [ cls | cls <- lp, clHead cls == clHead cl ]


-- | The number of clauses that have the same atom in their head as the given
-- clause for every clause in a given logic program. 
clsSameHeads :: LP -> [Int]
clsSameHeads lp = map (\x -> clSameHeads x lp) lp


-- | The base for the value A_min. 
aminBase :: LP -> Int -> Float
aminBase lp maxBH = (fromIntegral (maxBH - 1) / fromIntegral (maxBH + 1))


-- | The base for weight of the connections in neural network for a given logic
-- program. 
wBase :: LP -> Float -> Float -> Int -> Float -> Int -> Int -> Float
wBase lp amin r l beta maxBodies maxHeads = maximum [fstCondition, sndCondition]
    where
        fstCondition = (2 / beta) * (((log $ 1 + amin) - (log $ 1 - amin)) / ((fromIntegral maxBodies) * (amin - 1) + amin + 1))
        sndCondition = (2 / beta) * (((log $ 1 + amin) - (log $ 1 - amin) - (r * (fromIntegral $ l + 1)) ) / ((fromIntegral maxHeads) * (amin - 1) + amin + 1))


-- | List of atoms that "overlap", i.e. atoms that have the same index number
-- but one of them has label with "h" and the other does not.
overlappingAtoms :: LP -> [(Atom, Atom)]
overlappingAtoms lp = [ (atom, atomCouterpart) |
    atom <- snd partitionedAtoms,
    atomCouterpart <- fst partitionedAtoms,
    LogicPrograms.idx atom == LogicPrograms.idx atomCouterpart ]
    where
        -- atoms with "h" in the label and atoms without "h" in the label
        partitionedAtoms = partition (\x -> elem 'h' (LogicPrograms.label x)) (bp lp)


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


baseNNsteps :: [(Clause, Int, Int)] -> NeuralNetwork -> Float -> Float -> [(Atom, Atom)] -> NeuralNetwork
baseNNsteps [] nn amin w ovrl     = nn
baseNNsteps (t:ts) nn amin w ovrl = baseNNsteps ts newNN amin w ovrl
    where
        newNN = mergeNNupd nn (nnUpdFromTriple nn t amin w ovrl)


nnUpdFromTriple :: NeuralNetwork -> (Clause, Int, Int) -> Float -> Float -> [(Atom, Atom)] -> NNupdate
nnUpdFromTriple nn (cl, bdLen, sameHds) amin w ovrl = case cl of
    Fact _   -> updFromFact cl nn outBias ovrl w
    Cl _ _ _ -> updFromClause cl nn outBias hidBias w
    where
        outBias = w * (1 + amin) * (1 - fromIntegral sameHds) / 2
        hidBias = w * (1 + amin) * (fromIntegral bdLen - 1) / 2


updFromFact :: Clause -> NeuralNetwork -> Float -> [(Atom, Atom)] -> Float -> NNupdate
updFromFact (Fact hd) nn outBias ovrl w = case outNeuOld of
    Nothing
        | (elem hd ovrlH) || (not $ null inpNeuron) ->
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
        ovrlH        = map snd ovrl


updFromClause :: Clause -> NeuralNetwork -> Float -> Float -> Float -> NNupdate
updFromClause (Cl hd pBod nBod) nn outBias hidBias w = case outNeuOld of
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
        inpToHidConns = createInpToHidConn hidNeuIdx (inputNs ++ inpLayer nn) pBod nBod w
        

createInpNeurons :: [Atom] -> Int -> [Neuron] -> [Neuron]
createInpNeurons [] _ _                = []
createInpNeurons (a:as) idxStart inpNs = case findNeuByLabel a inpNs of
    Nothing -> mkInpNeu a : createInpNeurons as (idxStart + 1) inpNs
    Just _  -> createInpNeurons as idxStart inpNs
    where
        mkInpNeu = \x -> Neuron (show x) "idem" 0.0 ("inp" ++ show idxStart)


createOutNeurons :: [Atom] -> Int -> [Neuron] -> [Neuron]
createOutNeurons [] _ _                = []
createOutNeurons (a:as) idxStart outNs = case findNeuByLabel a outNs of
    Nothing -> mkOutNeu a : createOutNeurons as (idxStart + 1) outNs
    Just _  -> createOutNeurons as idxStart outNs
    where
        mkOutNeu = \x -> Neuron (show x) "tanh" 0.0 ("out" ++ show idxStart)


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
    }


recursiveConnections :: NeuralNetwork -> [(Atom, Atom)] -> NeuralNetwork
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
        remJust = \(Just x) -> x
        tupleAToN = \(x, y) -> (remJust $ findNeuByLabel x (outLayer nn), remJust $ findNeuByLabel y (outLayer nn))
        ovrlNs = map tupleAToN ovrl
        notOvrlNs = [ n |
            n <- outLayer nn,
            not $ elem (NeuralNetworks.label n) (map show $ fst $ unzip ovrl),
            not $ elem (NeuralNetworks.label n) (map show $ snd $ unzip ovrl) ]
        regularRecConns = createRecConnNormal (inpLayer nn) notOvrlNs
        abnormalRecConns = fst $ createRecConnAbnormal (inpLayer nn) ovrlNs
        recursiveNs = snd $ createRecConnAbnormal (inpLayer nn) ovrlNs
        recursiveConns = regularRecConns ++ abnormalRecConns


createRecConnNormal :: [Neuron] -> [Neuron] -> [Connection]
createRecConnNormal inpL outL =
    [ Connection (NeuralNetworks.idx outN) (NeuralNetworks.idx inpN) 1 |
        inpN <- inpL,
        outN <- outL,
        NeuralNetworks.label inpN == NeuralNetworks.label outN ]


createRecConnAbnormal :: [Neuron] -> [(Neuron, Neuron)] -> ([Connection], [Neuron])
createRecConnAbnormal inpL ovrlN = foldl mergeTriCN ([], []) triplesCN
    where
        remJust    = \(Just x) -> x
        tri        = \(x, y) -> (remJust $ find (\z -> NeuralNetworks.label x == NeuralNetworks.label z) inpL, x, y)
        triplesN   = map tri ovrlN
        triplesCN  = map recConnFromTriple triplesN
        mergeTriCN = \(cs1, ns1) (cs2, ns2) -> (cs1 ++ cs2, ns1 ++ ns2)


recConnFromTriple :: (Neuron, Neuron, Neuron) -> ([Connection], [Neuron])
recConnFromTriple (inpN, outN1, outN2) = ([c1, c2, c3], [n])
    where
        n  = Neuron ("rec" ++ NeuralNetworks.label outN1) "k" 0.0 ("rec" ++ NeuralNetworks.label outN1)
        c1 = Connection (NeuralNetworks.idx outN1) (NeuralNetworks.idx n) 1
        c2 = Connection (NeuralNetworks.idx outN2) (NeuralNetworks.idx n) 1
        c3 = Connection (NeuralNetworks.idx n) (NeuralNetworks.idx inpN) 1


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
    Nothing -> Connection "hidT" (NeuralNetworks.idx n) r : hidTToOutConns nn ns rs
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
            Just inpNeu -> [ Connection (NeuralNetworks.idx inp) addHidNeuIdx w | (w, inp) <- zip rs (inpLayer nn \\ [inpNeu, inpT]) ]
        mkHidToOutConn  = Connection addHidNeuIdx (NeuralNetworks.idx n) r
        addHidNeuIdx    = NeuralNetworks.idx mkAddHidNeu
        inpT            = last $ inpLayer nn
        newrs           = drop (l * (inpLen-1)^2) rs
        inpLen          = length (inpLayer nn)

findConnByNeu :: Neuron -> [Connection] -> Maybe Connection 
findConnByNeu outNeu hidToOutConns = find (\x -> from x == "hidT" && NeuralNetworks.idx outNeu == to x) hidToOutConns


findInpOut :: NeuralNetwork -> Neuron -> Maybe Neuron
findInpOut nn n = find (\x -> NeuralNetworks.label x == NeuralNetworks.label n) (inpLayer nn) 


rand :: Float -> IO [Float]
rand r = newStdGen >>= return . randomRs (-r, r)


additionalConnectionsIO :: NeuralNetwork -> Int -> Float -> Float -> IO NeuralNetwork
additionalConnectionsIO nn l ba r = do 
    rs <- rand r
    return (additionalConnections nn l ba rs)


showNNPython :: NeuralNetwork -> IO String
showNNPython x = do
    nn <- additionalConnectionsIO x 1 0.4 0.2
    return $ nnToPythonString nn
