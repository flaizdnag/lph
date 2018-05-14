{-|
Module      : Operator
Description : Definition of the immediate consequence operator for logic
              programs and tools for its iterations.
Copyright   : (c) Aleksandra Cz., 2017
                  Kinga O., 2017
                  Andrzej G., 2017
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Longer description
-}
module Operator
    ( isSublist
    , isElem
    , opTp
    , iterTp
    , upArrow
    )where

import Formulas
--import GHC.Exts

-- | checks if list is a sublist of other list
isSublist :: Eq a => [a] -> [a] -> Bool
isSublist [] _ = True
isSublist (x:xs) ys
                    | elem x ys = isSublist xs ys
                    | otherwise = False


-- | checks if list contains any element of other list
isElem :: Eq a => [a] -> [a] -> Bool
isElem [] _ = False
isElem (x:xs) ys
                | elem x ys = True
                | otherwise = isElem xs ys

-- | immediate consequence operator
opTp :: LogicP -> [Atom] -> [Atom]
opTp [] _      = []
opTp (x:xs) ys = case x of
                    (h, pos, neg) -> if isSublist pos ys && isElem neg ys == False
                                     then h : opTp xs ys
                                     else opTp xs ys

-- | iteraters Tp
iterTp :: LogicP -> [[Atom]] -> [[Atom]]
iterTp x (y:ys) 
               | (opTp x y) == y = (y:ys)
               | otherwise       = iterTp x ((opTp x y) : y:ys)

-- | initiates iterations of Tp and returns final result (Herbrand model) 
upArrow :: LogicP -> [Atom]
upArrow x = head (iterTp x [[]])



{-
interps :: LogicP -> [[Atom]]
interps x = sortWith length $ subsequences (bP x)
-}



