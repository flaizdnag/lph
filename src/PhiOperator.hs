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
jTop lp int = [ clHead cl | cl <- lp, evalBodyLukasiewicz cl int == Tr3v ]

jBot :: LP -> IntLP -> [Atom]
jBot lp int = filter falseBodies (lpHeads lp)
    where
        falseBodies x = all isBodyFa (atomDef x lp)
        isBodyFa x    = evalBodyLukasiewicz x int == Fa3v

phiOp :: LP -> IntLP -> IntLP
phiOp lp int = IntLP (jTop lp int) (jBot lp int)

stepPhiOp :: LP -> [IntLP] -> [IntLP]
stepPhiOp lp (i:is)
    | phiOp lp i == i = (i:is)
    | otherwise       = stepPhiOp lp ((phiOp lp i) : i:is)

iterPhiOp :: LP -> [IntLP]
iterPhiOp lp = stepPhiOp lp [IntLP [] []]

findModelPhiOp :: LP -> IntLP
findModelPhiOp = head . iterPhiOp

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

lp1 :: LP
lp1 = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 3 "") [A 1 ""] []]

cl1 :: Clause
cl1 = Cl (A 2 "") [A 1 ""] [A 3 ""]

lp2 :: LP
lp2 = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 3 "") [A 1 ""] [], Assumption (A 1 "")]

lp3 ::LP
lp3 = [Assumption (A 1 ""), Cl (A 2 "") [A 1 ""] []]

cl2 :: Clause
cl2 = Cl (A 2 "") [A 1 ""] []

lp4 :: LP
lp4 = [Fact (A 1 ""), Cl (A 2 "") [A 1 ""] []]

cl3 :: Clause
cl3 = Cl (A 2 "") [] [A 1 ""]

cl4 :: Clause
cl4 = Cl (A 2 "") [] [A 1 "", A 3 ""]


p3 :: LP
p3 = [Cl (A 1 "") [A 2 ""] [], Cl (A 1 "") [A 3 ""] [], Assumption (A 3 "")]

p4 :: LP
p4 = [Fact (A 3 ""), Assumption (A 3 "")]

p5 :: LP
p5 = [Cl (A 1 "") [A 2 ""] [A 3 ""], Assumption (A 3 ""), Fact (A 2 "")]

p6 :: LP
p6 = [Cl (A 1 "") [A 2 ""] []]
