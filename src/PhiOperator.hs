{-|
Module      : PhiOperator
Description : Phi operator implementation.
Copyright   : (c) Andrzej G., 2017-
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Longer description
-}
module PhiOperator
    ( jTop
    , jBot
    , opPhi
    , stepOpPhi
    , iterOpPhi
    , findModelOpPhi
    ) where

import LogicPrograms
import ThreeValuedSem
import Auxiliary


jTop :: LP -> IntLP -> [Atom]
jTop lp int = [ clHead cl | cl <- lp, evalBodyLukasiewicz cl int == Tr3v ]


jBot :: LP -> IntLP -> [Atom]
jBot lp int = filter falseBodies (lpHeads lp)
    where
        falseBodies x = all isBodyFa (atomDef x lp)
        isBodyFa x    = evalBodyLukasiewicz x int == Fa3v


opPhi :: LP -> IntLP -> IntLP
opPhi lp int = IntLP (jTop lp int) (jBot lp int)


stepOpPhi :: LP -> [IntLP] -> [IntLP]
stepOpPhi lp (i:is)
    | opPhi lp i == i = (i:is)
    | otherwise       = stepOpPhi lp ((opPhi lp i) : i:is)


iterOpPhi :: LP -> [IntLP]
iterOpPhi lp = stepOpPhi lp [IntLP [] []]


findModelOpPhi :: LP -> IntLP
findModelOpPhi = head . iterOpPhi
