{-|
Module      : Operator
Description : Definition of the immediate consequence operator for logic
              programs and tools for its iterations.
Copyright   : (c) Aleksandra Cz., 2017--2018
                  Kinga O., 2017--2018
                  Andrzej G., 2017--2018
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Longer description
-}
module Operator
    ( opTp
    , opTp'
    , upArrow
    , upArrow'
    , isConsequenceA
    ) where

import Formulas
import Auxiliary
import Data.List

-- | immediate consequence operator
opTp :: LogicP -> [Atom] -> [Atom]
opTp [] _      = []
opTp (x:xs) ys = case x of
                    (h, pos, neg) -> if isSublist pos ys && isElem neg ys == False
                                     then nub (h : opTp xs ys)
                                     else opTp xs ys

opTp' :: LogicP -> [Atom] -> [Atom]
opTp' [] _      = []
opTp' (x:xs) ys
    | con_bodyP && con_bodyN    = nub (hClHead x ++ opTp' xs ys)
    | otherwise                 = opTp' xs ys
        where
            con_bodyP = isSublist (hClBodyP x) ys
            con_bodyN = not (isElem (hClBodyN x) ys)

opTp3 :: LogicP -> [Atom] -> [Atom]
opTp3 xs i = nub [hclh | h <- xs, hclh <- hClHead h, isSublist (hClBodyP h) i, not (isElem (hClBodyN h) i)]

-- | iteraters Tp
iterTp :: LogicP -> [[Atom]] -> [[Atom]]
iterTp x (y:ys) 
               | (opTp x y) == y = (y:ys)
               | otherwise       = iterTp x ((opTp x y) : y:ys)

iterTp' :: LogicP -> [Atom] -> [Atom]
iterTp' xs ys
    | (opTp' xs ys) == ys = ys
    | otherwise           = iterTp' xs (opTp' xs ys)

iterTp3 :: LogicP -> [[Atom]] -> [[Atom]]
iterTp3 x (y:ys) 
               | (opTp3 x y) == y = (y:ys)
               | otherwise       = iterTp3 x ((opTp3 x y) : y:ys)

-- | initiates iterations of Tp and returns final result (Herbrand model) 
upArrow :: LogicP -> [Atom]
upArrow x = head (iterTp x [[]])

upArrow' :: LogicP -> [Atom]
upArrow' xs = iterTp' xs []

upArrow3 :: LogicP -> [Atom]
upArrow3 x = head (iterTp3 x [[]])

-- | checks if an atom is a logical consequence of a logic program
isConsequenceA :: LogicP -> Atom -> Bool
isConsequenceA xs a = if elem a (upArrow xs)
                         then True 
                      else False



{-
interps :: LogicP -> [[Atom]]
interps x = sortWith length $ subsequences (bP x)
-}
