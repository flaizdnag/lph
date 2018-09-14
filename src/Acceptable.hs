{-|
Module      : Acceptable
Description : Tools needed to check if a logic program is acceptable.
Copyright   : (c) Aleksandra Cz., 2017
                  Kinga O., 2017
                  Andrzej G., 2017
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Module which contains functions that allow to check if a given logic program is
acceptable.
-}
module Acceptable
    ( negP
    , logicP'
    , isAcceptable
    ) where

import Formulas
import Graph
import Completion
import LvlMap
import Data.List  (nub, foldl1', subsequences, sort, (\\), intersect)

-- | The list of all atoms that some atom occurring negatively in a logic
-- program depends on.
negP :: LogicP -> [Atom]
negP lp = intsToAtoms $ nub (concatMap (dependsOn g) nodes)
    where
        g       = graph lp
        nodes   = atomsToInts (bPBodiesN lp)

-- | Creates a program from the initial logic program with Horn clauses whose
-- heads belong to @negP@.
logicP' :: LogicP -> LogicP
logicP' lp = [ hcl | hcl <- lp, elem (hClHead hcl) (negP lp) ]

-- | Makes a list of models for a given logic program from a given list of
-- atoms. The assumption here is that the list contains atoms that are mapped to
-- 'true' and is left without a change.
alTomsLP :: LogicP -> [Atom] -> [IntLP]
alTomsLP lp as = map makeInt [ bs | bs <- subsequences ((bP lp) \\ as), modelCheckLP lp (bs ++ as, []) ]
    where
        makeInt = \xs -> alTointLP xs lp

-- | Makes an interpretation from a list of atoms (that are assumed to be
-- 'true').
alTointLP :: [Atom] -> LogicP -> IntLP
alTointLP as lp = (as, (bP lp) \\ as)

-- | Makes a list of interpretation for a given logic program that are models
-- for the logic program and in the same time are models for the Clark's
-- completion of the negative version of the logic program ('logicP'') when
-- restricted only to atoms that occur in 'negP'.
candidateInts :: LogicP -> [IntLP]
candidateInts lp = concatMap makeMsLP filtered
    where
        -- creating Clark's completion for the logic program 'logicP'
        compP'      = comp (logicP' lp)
        -- creating a list of all subsequences of the list of atoms 'negP'
        subseq      = subsequences (negP lp)
        -- filtering the list 'subseq' an leaving only those that are models for the 'compP''
        filtered    = filter (\as -> compP'ms as) subseq
        -- condition for the 'filtered'; checks if a given list of atoms is a model for 'compP''
        compP'ms    = \xs -> modelCheckCPL compP' $ intLPTointCPL $ alTointLP xs lp
        -- makes a list of models for a logic program 'lp' from a given list of atoms
        makeMsLP    = \xs -> alTomsLP lp xs

notCons :: HClause -> IntLP -> [Atom]
notCons hcl (tr, fa) = intersect (hClBodyPDup hcl) fa ++ intersect (hClBodyNDup hcl) tr

-- | Takes a Horn clause, a level mapping and an interpretation. Checks if the
-- condition is fulfilled for a horn clause, i.e. ...
conditionHCl :: HClause -> [(Atom, Int)] -> IntLP -> Bool
conditionHCl hcl lvlM int
    | null areNotCons   = all isSmaller (hClBodyDup hcl)
    | otherwise         = any isSmaller areNotCons
    where
        areNotCons = notCons hcl int
        isSmaller  = \x -> lvlMVal x lvlM < lvlMVal (hClHead hcl) lvlM

conditionLP :: LogicP -> [(Atom, Int)] -> IntLP -> Bool
conditionLP lp lvlM int = all (\x -> conditionHCl x lvlM int) lp

conditionInts :: LogicP -> [(Atom, Int)] -> [IntLP] -> Bool
conditionInts lp lvlM ints = any (\x -> conditionLP lp lvlM x) ints

conditionLvlMs :: LogicP -> [[(Atom, Int)]] -> [IntLP] -> Bool
conditionLvlMs lp lvlMs ints = any (\x -> conditionInts lp x ints) lvlMs

isAcceptable :: LogicP -> Bool
isAcceptable lp = conditionLvlMs lp (possibleLvLMaps lp) (candidateInts lp)
