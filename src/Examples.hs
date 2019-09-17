{-|
Module      : Examples
Description : Examples of Horn clauses.
Copyright   : (c) Aleksandra Cz., 2017
                  Kinga O., 2017
                  Andrzej G., 2017
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Longer description
-}
module Examples
    ( lp1
    , lp2
    , lpDr
    , lpDr'
    , cl1
    , cl2
    , cl3
    , cl4
    , cl5
    , lp2NN
    --, lp2NNrec
    --, lp2NNadd
    , lpDrNNbase
    , lpDrNNadd
    , lpDrNNfull
    ) where

import LogicPrograms
import NeuralNetworks
import TranslationTp
import TpOperator


-- P1 = {
--      A2 <- A1 , A4
--      A1 <- A3
--      A5 <-
-- }
lp1 :: LP
lp1 = [Cl (A 2 "") [A 1 "", A 4 ""] [], Cl (A 1 "") [A 3 ""] [], Fact (A 5 "")]


-- cl1 : A2 <- A3 , A4
cl1 :: Clause
cl1 = Cl (A 2 "") [A 3 "", A 4 ""] []


-- cl2 : A2 <- A3 , A4 , A6
cl2 :: Clause
cl2 = Cl (A 2 "") [A 3 "", A 4 "", A 6 ""] []


-- cl3 : A6 <- A3 , A4
cl3 :: Clause
cl3 = Cl (A 6 "") [A 3 "", A 4 ""] []


-- P2 = {
--      A2 <- A1 , ~A4
--      A3 <- A1
-- }
--lp2 :: LP
--lp2 = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 3 "") [A 1 ""] []]

lp2NN :: NeuralNetwork
lp2NN = TranslationTp.baseNN lp2 (NNfactors 1 2 0.05 0.0 0.5 0.5)

lp2 :: LP
lp2 =
    [ Cl (A 1 "") [A 10 "", A 10 ""] [A 11 "", A 11 ""]
    , Cl (A 2 "") [A 15 "", A 15 ""] [A 16 "", A 16 ""]
    , Fact (A 1 ""), Assumption (A 1 "")
    , Fact (A 2 ""), Assumption (A 2 "")
    ]
--lp2NNrec :: NeuralNetwork
--lp2NNrec = recursiveConnections lp2NN (overlappingAtoms lp2)

--lp2NNadd :: IO NeuralNetwork
--lp2NNadd = additionalNN lp2NNrec (NNfactors 1 2 0.05 0.0 0.5 0.5) [A 5 "", A 6 ""]


-- cl4 : A2 <- A1 , ~A4
cl4 :: Clause
cl4 = Cl (A 2 "") [A 1 ""] [A 4 ""]


-- cl5 : A2 <- A1 , ~A3
cl5 :: Clause
cl5 = Cl (A 2 "") [A 1 ""] [A 3 ""]


-- Pd = {
--      A1 <- A2 , A3
--      A4 <- A5
--      A6 <- T
-- }
-- 
-- Pd' = {
--      A1 <- A2 , A3
--      A4 <- A5
--      A6 <- T
--      A7 <- A4
--      A5 <- T
-- }
lpDr :: LP
lpDr = [ Cl (A 1 "") [A 2 "", A 3 ""] [],
         Cl (A 4 "") [A 5 ""] [],
         Fact (A 6 "") ]

lpDr' :: LP
lpDr' = [ Cl (A 1 "") [A 2 "", A 3 ""] [],
          Cl (A 4 "") [A 5 ""] [],
          Fact (A 6 ""),
          Cl (A 7 "") [A 4 ""] [],
          Fact (A 5 "") ]

lpDrNNbase :: NeuralNetwork
lpDrNNbase = TranslationTp.baseNN lpDr (NNfactors 1 1 0.05 0.0 0.5 0.5)

lpDrNNadd :: IO NeuralNetwork
lpDrNNadd = TranslationTp.additionalNN lpDrNNbase (NNfactors 1 1 0.05 0.0 0.5 0.5) [A 4 "", A 7 ""]

{-
lpDrNNa7 :: NeuralNetwork
lpDrNNa7 = mergeNNupd lpDrNN upd
    where
        upd = NNupdate
            { inpNeuToAdd      = [Neuron "A7" "idem" 0.0 "inp7"]
            , hidNeuToAdd      = []
            , outNeuToAdd      = [Neuron "A7" "idem" 0.0 "out7"]
            , outNeuToRemove   = []
            , inpToHidConToAdd = []
            , hidToOutConToAdd = []
            }

lpDrNNrec :: NeuralNetwork
lpDrNNrec = recursiveConnections lpDrNNa7 (overlappingAtoms lpDr)
-}

lpDrNNfull :: IO NeuralNetwork
lpDrNNfull = do
    nn <- lpDrNNadd
    return $ recursiveConnections nn (overlappingAtoms lpDr [A 4 "", A 7 ""])



{-
-- from LogicPrograms
lp1 :: LP
lp1 = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 3 "") [A 1 ""] []]

cl1 :: Clause
cl1 = Cl (A 2 "") [A 1 ""] [A 3 ""]

lp2 :: LP
lp2 = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 3 "") [A 1 ""] [], Assumption (A 1 "")]

lp3 ::LP
lp3 = [Assumption (A 1 ""), Cl (A 2 "") [A 1 ""] []]

cl2 :: Clause
cl2 = Cl (A 2 "") [A 1 ""] []

lp4 :: LP
lp4 = [Fact (A 1 ""), Cl (A 2 "") [A 1 ""] []]

cl3 :: Clause
cl3 = Cl (A 2 "") [] [A 1 ""]

lp5 :: LP
lp5 = [Cl (A 1 "") [] [A 2 ""], Assumption (A 2 "")]

cl4 :: Clause
cl4 = Fact (A 1 "")

lp6 :: LP
lp6 = [Fact (A 1 ""), Cl (A 2 "") [] [A 1 ""]]

cl5 :: Clause
cl5 = Cl (A 2 "") [] [A 1 "", A 3 ""]

-- from Completion

p1a :: LP
p1a = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 1 "") [A 3 ""] [], Fact (A 5 "")]

-- from PhiOperator
lp1' :: LP
lp1' = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 3 "") [A 1 ""] []]

cl1' :: Clause
cl1' = Cl (A 2 "") [A 1 ""] [A 3 ""]

lp2' :: LP
lp2' = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 3 "") [A 1 ""] [], Assumption (A 1 "")]

lp3' ::LP
lp3' = [Assumption (A 1 ""), Cl (A 2 "") [A 1 ""] []]

cl2' :: Clause
cl2' = Cl (A 2 "") [A 1 ""] []

lp4' :: LP
lp4' = [Fact (A 1 ""), Cl (A 2 "") [A 1 ""] []]

cl3' :: Clause
cl3' = Cl (A 2 "") [] [A 1 ""]

cl4' :: Clause
cl4' = Cl (A 2 "") [] [A 1 "", A 3 ""]


p3 :: LP
p3 = [Cl (A 1 "") [A 2 ""] [], Cl (A 1 "") [A 3 ""] [], Assumption (A 3 "")]

p4 :: LP
p4 = [Fact (A 3 ""), Assumption (A 3 "")]

p5 :: LP
p5 = [Cl (A 1 "") [A 2 ""] [A 3 ""], Assumption (A 3 ""), Fact (A 2 "")]

p6 :: LP
p6 = [Cl (A 1 "") [A 2 ""] []]

-- from Translation
p1'' :: LP
p1'' = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 1 "") [A 3 ""] [], Fact (A 5 "")]

--p1NN'' :: NeuralNetwork
--p1NN'' = baseNN p1 0.5 0.5 1 0.0 0.05 2 


p2'' :: LP
p2'' = p1'' ++ [Fact (A 2 "h")]

--p2NN'' :: NeuralNetwork
--p2NN'' = baseNN p2 0.5 0.5 1 0.0 0.05 2

--p2NNrec'' :: NeuralNetwork
--p2NNrec'' = recursiveConnections p2NN (overlappingAtoms p2)

--p2NNadd'' :: IO NeuralNetwork
--p2NNadd'' = additionalConnectionsIO p2NNrec 1 0.0 0.05

p3'' :: LP
p3'' = [Cl (A 1 "") [A 2 ""] [A 3 ""], Cl (A 10 "") [A 2 ""] [A 3 ""]]

--p3NN'' :: NeuralNetwork
--p3NN'' = baseNN p3 0.5 0.5 1 0.0 0.05 2

--p3NNrec'' :: NeuralNetwork
--p3NNrec'' = recursiveConnections p3NN (overlappingAtoms p3)

--p3NNadd'' :: IO NeuralNetwork
--p3NNadd'' = additionalConnectionsIO p3NNrec 2 0.4 4

p4'' :: LP 
p4'' = [Cl (A 1 "")[A 2 "", A 3 ""][], Cl (A 2 "")[A 3 ""][], Cl (A 2 "")[A 1 ""][]]

--p4NN'' :: NeuralNetwork
--p4NN'' = baseNN p4 0.5 0.5 1 0.0 0.05 2

--p4NNrec'' :: NeuralNetwork
--p4NNrec'' = recursiveConnections p4NN (overlappingAtoms p4)

--p4NNadd'' :: IO NeuralNetwork
--p4NNadd'' = additionalConnectionsIO p4NNrec 2 0.0 0.2

p5'' :: LP
p5'' = [Cl (A 1 "") [A 2 ""] [A 3 ""], Cl (A 2 "") [A 4 ""] [], Fact (A 4 "")]

--p5NN'' :: NeuralNetwork
--p5NN'' = baseNN p5 0.5 0.5 1 0.0 0.05 2

--p5NNrec'' :: NeuralNetwork
--p5NNrec'' = recursiveConnections p5NN (overlappingAtoms p5)

--p5NNadd'' :: IO NeuralNetwork
--p5NNadd'' = additionalConnectionsIO p5NNrec 1 0.4 0.2





exl = [(A 1, [A 2], [A 3]), (A 4, [A 6], [A 5]), (A 4, [A 1], [A 7]), (A 2, [], [])]
ex1 = [(A 1, [], []), (A 2, [], [A 3]), (A 4, [A 1], [A 3])]
ex1a = [(A 1, [], []), (A 2, [], [A 3]), (A 4, [A 1, A 3], [])]
ex2 = [(A 1, [A 3, A 5], [A 2, A 7]), (A 8, [A 9], [A 10, A 11]), (A 12, [A 4, A 6], [A 13, A 11])]
ex3 = [(A 1, [A 3, A 5], [A 2, A 7]), (A 8, [A 9], [A 10, A 11]), (A 1, [A 4, A 6], [A 13, A 11])]
ex4 = [(A 1, [A 3, A 5], [A 2, A 7]), (A 8, [A 9], [A 10, A 11]), (A 12, [A 4, A 6], [A 13, A 11]), (A 1, [A 3, A 5], [A 2, A 7]), (A 8, [A 9], [A 10, A 11]), (A 12, [A 4, A 6], [A 13, A 11])]
ex5 = [(A 1, [], []), (A 2, [], [A 3]), (A 4, [A 1], [A 3]), (A 2, [A 1], [A 3]), (A 3, [A 12], [A 33]), (A 3, [A 1], [A 3]), (A 1, [A 31], [A 3])]
ex6 = [(A 4, [A 5], []), (A 1, [], [A 2, A 3]), (A 2, [], [A 4])]
ex7 = [(A 4, [A 5], []), (A 12, [], []), (A 1, [], [A 2, A 3]), (A 2, [], [A 4]), (A 6, [], [])]
ex8 = [(A 10, [], []), (A 4, [A 5], []), (A 12, [], []), (A 1, [], [A 2, A 3]), (A 2, [], [A 4]), (A 6, [], [])]
ex9 = [(A 10, [], []), (A 4, [A 5], []), (A 4, [A 19], [A 11]), (A 12, [], []), (A 1, [], [A 2, A 3]), (A 2, [], [A 4]), (A 6, [], [])]
ex9a = [(A 1, [], []), (A 3, [], [A 1]), (A 3, [], [])]
-- no T
ex10 = [(A 10,[],[A 19]),(A 4,[A 5],[]),(A 12,[],[A 22]),(A 1,[],[A 2,A 3]),(A 2,[],[A 4]),(A 6,[],[A 18])]
-- no N
ex11 = [(A 10,[],[A 4]),(A 4,[A 12],[]),(A 12,[],[]),(A 2,[],[A 2,A 4]),(A 6,[],[A 10])]

--loop
--ex3 = [(A 1, [A 3], []), (A 2, [A 1], [A 4]), (A 3, [], [A 2])]


--program
p1 = [(A 1, [A 2], [A 3]), (A 1, [], [A 4]), (A 2, [A 5], []), (A 5, [], [])]
--compP, logicP'

{-
trueE (E (V (A 1)) (D [C [V (A 2),N (V (A 3))]])) ([V (A 5)],[V (A 3),V (A 4)])
interpretation [[E (V (A 1)) (D [C [V (A 2),N (V (A 3))],N (V (A 4))]),E (V (A 2)) (V (A 5))],[E (V (A 5)) T],[N (V (A 3)),N (V (A 4))]]
interpretation (groupByValue (compP p1))
-}
-}
