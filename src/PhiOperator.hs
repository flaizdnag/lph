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
    , phiOp
    , stepPhiOp
    , iterPhiOp
    , findModelPhiOp
    ) where

import LogicPrograms
import ThreeValuedSem
import Auxiliary


jTop :: LP -> IntLP -> [Atom]
jTop lp int = [ clHead cl | cl <- lp, evalBody cl int == Tr3v ]

jBot :: LP -> IntLP -> [Atom]
jBot lp int = filter falseBodies (lpHeads lp)
    where
        falseBodies = \x -> all isBodyFa (atomDef x lp)
        isBodyFa    = \x -> evalBody x int == Fa3v

phiOp :: LP -> IntLP -> IntLP
phiOp lp int = Int (jTop lp int) (jBot lp int)

stepPhiOp :: LP -> [IntLP] -> [IntLP]
stepPhiOp lp (i:is)
    | phiOp lp i == i = (i:is)
    | otherwise       = stepPhiOp lp ((phiOp lp i) : i:is)

iterPhiOp :: LP -> [IntLP]
iterPhiOp lp = stepPhiOp lp [Int [] []]

findModelPhiOp :: LP -> IntLP
findModelPhiOp = head . iterPhiOp

p3 :: LP
p3 = [Cl (A 1 "") [A 2 ""] [], Cl (A 1 "") [A 3 ""] [], Assumption (A 3 "")]

p4 :: LP
p4 = [Fact (A 3 ""), Assumption (A 3 "")]

p5 :: LP
p5 = [Cl (A 1 "") [A 2 ""] [A 3 ""], Assumption (A 3 ""), Fact (A 2 "")]

p6 :: LP
p6 = [Cl (A 1 "") [A 2 ""] []]
