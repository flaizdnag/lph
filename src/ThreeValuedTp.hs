module ThreeValuedTp () where

import Formulas
import Completion
import Auxiliary

evalAtom3LP :: Atom -> IntLP -> Bool3
evalAtom3LP a (tr, fa)
    | elem a tr = Tr
    | elem a fa = Fa
    | otherwise = Un

evalHCl3 :: HClause -> IntLP -> Bool3
evalHCl3 (h, pb, nb) (tr, fa) = case evalAtom3LP h (tr, fa) of
    Tr              -> Tr
    Fa
        | isTrBody  -> Fa
        | isFaBody  -> Tr
        | otherwise -> Un
    Un
        | isUnBody  -> Un
        | otherwise -> Tr
    where
        isTrBody = isSublist pb tr && isSublist nb fa
        isFaBody = jointElem pb fa || jointElem nb tr
        isUnBody = not isTrBody && not isFaBody

evalBodyHcl3 :: HClause -> IntLP -> Bool3
evalBodyHcl3 (h, pb, nb) (tr, fa)
    | isSublist pb tr && isSublist nb fa = Tr
    | jointElem pb fa || jointElem nb tr = Fa
    | otherwise                          = Un

jTop :: LogicP -> IntLP -> [Atom]
jTop lp int = [ hClHead hcl | hcl <- lp, evalBodyHcl3 hcl int == Tr ]

jBot :: LogicP -> IntLP -> [Atom]
jBot lp int = [ hClHead hcl | hcl <- lp, evalBodyHcl3 hcl int == Fa ]

phiP :: LogicP -> IntLP -> IntLP
phiP lp int = (jTop lp int, jBot lp int)

stepPhiP :: LogicP -> [IntLP] -> [IntLP]
stepPhiP lp (i:is)
    | phiP lp i == i = (i:is)
    | otherwise      = stepPhiP lp ((phiP lp i) : i:is)

iterPhiP :: LogicP -> [IntLP]
iterPhiP lp = stepPhiP lp [([], [])]

findModelPhiP :: LogicP -> IntLP
findModelPhiP = head . iterPhiP

p3 :: LogicP
p3 = [(A 1 "", [A 2 ""], []), (A 1 "", [A 3 ""], []), (A 3 "", [], [])]

p6 :: LogicP
p6 = [(A 1 "", [A 2 ""], [])]
