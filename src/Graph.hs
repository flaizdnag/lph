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

Longer description
-}
module Graph
    ( atomToInt
    , intToAtom
    , bounds'
    , zipEdges
    , edges'
    , graph
    ) where

import Formulas
import Operator
import Data.Graph
import Data.List

-- | takes list of Atoms and returns list of Ints
atomToInt :: [Atom] -> [Int]
atomToInt []     = []
atomToInt (x:xs) = case x of
    A b -> b : atomToInt xs

-- | takes list of Ints and returns list of Atoms
intToAtom :: [Int] -> [Atom]
intToAtom []     = []
intToAtom (x:xs) = case x of
    b -> A b : intToAtom xs

-- | takes Herbrand Base and returns bounds for the graph (requirement: Atoms in
-- program have to be numbered in order without any deficiencies)
bounds' :: LogicP -> (Int, Int)
bounds' xs = (minimum (atomsNum), maximum (atomsNum))
    where
        atomsNum = atomToInt (bP xs)


zipEdges :: [Int] -> [Int] -> [(Int, Int)]        
zipEdges _ []           = []
zipEdges (x:xs) (y:ys)  = (x, y) : zipEdges (x:xs) ys

-- | creates list of pairs, each containing HC Head and one element of HC Body
edges' :: LogicP -> [(Int, Int)]
edges' [] = []
edges' (x:xs) = nub (zipEdges (atomToInt (hClHead x)) (atomToInt (hClBody x)) ++ edges' xs)

-- | creates a graph
graph :: LogicP -> Graph
graph x = buildG (bounds' x) (edges' x)

--gr = buildG (1,7) [(1,2), (3,2), (4,5), (7,2), (6,5), (1,7), (2,4)]
