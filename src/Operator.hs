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
    , upArrow
    , isConsA
    ) where

import Formulas
import Auxiliary
import Data.List (nub)

-- | Immediate consequence operator Tp.
opTp :: LogicP -> [Atom] -> [Atom]
opTp hcls int = nub [hClHead h | h <- hcls,
                                 isSublist (hClBodyP h) int,
                                 not (jointElem (hClBodyNDup h) int)]

-- | Iterations of the Tp operator.
iterTp :: LogicP -> [[Atom]] -> [[Atom]]
iterTp x (y:ys) 
    | opTp x y == y = (y:ys)
    | otherwise     = iterTp x ((opTp x y) : y:ys)

-- | Iterates the Tp operator starting from the empty interpretation. Saves all
-- iterations as elements of the list (newest are at the beginning of the list). 
upArrow :: LogicP -> [[Atom]]
upArrow x = iterTp x [[]]

-- | Checks if an atom is a logical consequence of a logic program.
isConsA :: Atom -> LogicP -> Bool
isConsA a lp = elem a (head (upArrow lp))
