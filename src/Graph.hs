{-|
Module      : Graph
Description : Tools needed to create a graph for a logic program.
Copyright   : (c) Aleksandra Cz., 2017
                  Kinga O., 2017
                  Andrzej G., 2017
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Tools needed for creating a graph from a given logic program.
-}
module Graph
    ( atomsToInts
    , intsToAtoms
    , limits
    , lpEdges
    , graph
    , dependsOn
    ) where

import LogicPrograms
import TpOperator
import Data.Graph   (Graph, reachable, buildG)
import Data.List    (delete)
import Data.Array   (assocs)


-- | Takes a list of atoms and returns a list of their indexes.
atomsToInts :: [Atom] -> [Int]
atomsToInts = map idx


-- | Takes a list of indexes and returns a list of atoms with empty labels.
intsToAtoms :: [Int] -> [Atom]
intsToAtoms []     = []
intsToAtoms (x:xs) = (A x []) : intsToAtoms xs


-- | Takes the Herbrand Base of a logic program and returns the limits for the
-- number in the graph (requirement: Atoms in the program have to be numbered in
-- order without any deficiencies).
limits :: LP -> (Int, Int)
limits xs = (minimum idxs, maximum idxs)
    where
        idxs = atomsToInts (bpDup xs)


-- | Creates a list of pairs: the head of a  clause and an atom from the body of
-- the clause.
lpEdges :: LP -> [(Int, Int)]
lpEdges lp = [ (headIdx, bodyAtomIdx) |
    cl <- lp,
    atom <- clBody cl,
    let headIdx = idx (clHead cl),
    let bodyAtomIdx = idx atom ]


-- | Creates a graph for a logic program.
graph :: LP -> Graph
graph x = buildG (limits x) (lpEdges x)


-- | Creates a list of nodes that the given node depends on. The difference
-- between @depends@ and @reachable@ is that the node is not dependent from
-- itself, unless there is a set of edges that connect the node with itself.
-- NOTICE: the connections run in the opposite direction as in the definition
-- from PhD.
dependsOn :: Graph -> Int -> [Int]
dependsOn g n
    | selfDep g n   = reachable g n
    | otherwise     = delete n (reachable g n)


-- | Establishes if a given node depends on itself, i.e. if it is reachable from
-- nodes that are reachable from the given node apart from that node.
selfDep :: Graph -> Int -> Bool
selfDep g n = elem n $ concatMap (reachable g) (snd ((assocs g) !! (n - 1)))
