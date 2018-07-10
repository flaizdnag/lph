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
    , opTp''
    , upArrow
    , upArrow'
    )where

import Formulas
import Auxiliary
import Data.List

-- | immediate consequence operator
-- TODO remove duplicates from the result
opTp :: LogicP -> [Atom] -> [Atom]
opTp [] _      = []
opTp (x:xs) ys = case x of
                    (h, pos, neg) -> if isSublist pos ys && isElem neg ys == False
                                     then h : opTp xs ys
                                     else opTp xs ys

opTp' :: LogicP -> [Atom] -> [Atom]
opTp' [] _      = []
opTp' (x:xs) ys
    | con_bodyP && con_bodyN    = nub (hClHead x ++ opTp' xs ys)
    | otherwise                 = opTp' xs ys
        where
            con_bodyP = isSublist (hClBodyP x) ys
            con_bodyN = not (isElem (hClBodyN x) ys)

opTp'' :: LogicP -> [Atom] -> [Atom]
opTp'' hcls int = nub [ head (hClHead h) | h <- hcls, isSublist (hClBodyP h) int, not (isElem (hClBodyN h) int) ]

-- | iteraters Tp
iterTp :: LogicP -> [[Atom]] -> [[Atom]]
iterTp x (y:ys) 
               | (opTp x y) == y = (y:ys)
               | otherwise       = iterTp x ((opTp x y) : y:ys)

iterTp' :: LogicP -> [Atom] -> [Atom]
iterTp' xs ys
    | (opTp' xs ys) == ys = ys
    | otherwise           = iterTp' xs (opTp' xs ys)

-- | initiates iterations of Tp and returns final result (Herbrand model) 
upArrow :: LogicP -> [Atom]
upArrow x = head (iterTp x [[]])

upArrow' :: LogicP -> [Atom]
upArrow' xs = iterTp' xs []




{-
interps :: LogicP -> [[Atom]]
interps x = sortWith length $ subsequences (bP x)
-}
