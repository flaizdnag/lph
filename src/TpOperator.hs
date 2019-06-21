{-|
Module      : TpOperator
Description : Definition of the immediate consequence operator for logic
              programs and tools for its iterations.
Copyright   : (c) Aleksandra Cz., 2017-
                  Kinga O., 2017-
                  Andrzej G., 2017-
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Longer description
-}
module TpOperator
    ( opTp
    , upArrow
    , isConsequenceA
    ) where

import LogicPrograms
import Auxiliary
import TwoValuedSem
import Data.List (nub, (\\))


-- | Immediate consequence operator Tp.
opTp :: LP -> IntLP -> IntLP
opTp lp int = IntLP newTr newFa
    where
        newTr = [ clHead cl | cl <- lp, evalBody2v cl int == Tr2v ]
        newFa = (bp lp) \\ newTr


-- | Iterations of the Tp operator.
iterTp :: LP -> [IntLP] -> [IntLP]
iterTp lp (y:ys)
    | opTp lp y == y = (y:ys)
    | otherwise      = iterTp lp ((opTp lp y) : y:ys)


-- | Iterates the Tp operator starting from the empty interpretation. Saves all
-- iterations as elements of the list (newest are at the beginning of the list). 
upArrow :: LP -> [IntLP]
upArrow x = iterTp x [IntLP [] []]


-- | Checks if an atom is a logical consequence of a logic program by means of
-- the Tp operator.
isConsequenceA :: Atom -> LP -> Bool
isConsequenceA a lp = elem a (trLP $ head (upArrow lp))
