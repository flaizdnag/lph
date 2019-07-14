{-|
Module      : PhiOperator
Description : Phi operator implementation.
Copyright   : (c) Andrzej G., 2017-
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Module containing $\Phi$-operator definition along with function that iterates
the operator starting from the empty interpretation and function that searches
for a model of a logic program by means of the operator.
-}
module PhiOperator
    ( opPhi
    , iterOpPhi
    , findModelOpPhi
    ) where

import LogicPrograms
import ThreeValuedSem
import Auxiliary


-- | The first part of the operator---returns the set of atoms that are mapped
-- to true.
jTop :: LP -> IntLP -> [Atom]
jTop lp int = [ clHead cl | cl <- lp, evalBodyLukasiewicz cl int == Tr3v ]


-- | The second part of the operator---returns the set of atoms that are mapped
-- to false.
jBot :: LP -> IntLP -> [Atom]
jBot lp int = filter falseBodies (lpHeads lp)
    where
        falseBodies x = all isBodyFa (atomDef x lp)
        isBodyFa x    = evalBodyLukasiewicz x int == Fa3v


-- | Definition of the $\Phi$-operator.
opPhi :: LP -> IntLP -> IntLP
opPhi lp int = IntLP (jTop lp int) (jBot lp int)


-- | Iterations of the $\Phi$-operator that start from an empty interpretation.
iterOpPhi :: LP -> [IntLP]
iterOpPhi lp = stepOpPhi lp [IntLP [] []]
    where
        stepOpPhi lp (i:is)
            | opPhi lp i == i = (i:is)
            | otherwise       = stepOpPhi lp ((opPhi lp i) : i:is)


-- | Model for a logic program that is the least fixedpoint of $\Phi$-operator.
findModelOpPhi :: LP -> IntLP
findModelOpPhi = head . iterOpPhi
