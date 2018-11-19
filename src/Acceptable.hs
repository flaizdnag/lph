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

import LogicPrograms
import Graph
import Completion
import LvlMap
import CPL
import Data.List  (nub, foldl1', subsequences, sort, (\\), intersect)


-- | The list of all atoms that some atom occurring negatively in a logic
-- program depends on.
negP :: LP -> [Atom]
negP lp = intsToAtoms $ nub (concatMap (dependsOn g) nodes)
    where
        g     = graph lp
        nodes = atomsToInts (lpNBodies lp)

-- | Creates a program from the initial logic program with Horn clauses whose
-- heads belong to @negP@.
logicP' :: LP -> LP
logicP' lp = [ cl | cl <- lp, elem (clHead cl) (negP lp) ]

-- | Makes a list of interpretation for a given logic program that are models
-- for the logic program and in the same time are models for the Clark's
-- completion of the negative version of the logic program ('logicP'') when
-- restricted only to atoms that occur in 'negP'.
candidateInts :: LP -> [IntLP]
candidateInts lp = concatMap makeMsLP filtered
    where
        -- creating Clark's completion for the logic program 'logicP'
        compP'       = comp (logicP' lp)
        -- creating a list of all subsequences of the list of atoms 'negP'
        subseq       = subsequences (negP lp)
        -- filtering the list 'subseq' an leaving only those that are models for the 'compP''
        filtered     = filter (\as -> compP'ms as) subseq
        -- condition for the 'filtered'; checks if a given list of atoms is a model for 'compP''
        compP'ms     = \xs -> isModel2vCPL compP' $ intLPtoIntCPL $ asToIntLP xs lp
        -- turns a list of atoms that are assumed to be "true" into a list of
        -- all models for a logic program, that can be obtained from the list
        -- without modifying it
        makeMsLP = \as -> map makeInt [ bs |
            bs <- subsequences ((bp lp) \\ as),
            isModel2vLP lp (IntLP (bs ++ as) []) ]
            where
                makeInt = \xs -> asToIntLP xs lp
        -- turns a list of "true" atoms into an interpretation
        asToIntLP    = \as lp -> IntLP as ((bp lp) \\ as)

-- | Takes a Horn clause, a level mapping and an interpretation. Checks if the
-- condition is fulfilled for a horn clause, i.e. ...
conditionHCl :: Clause -> [(Atom, Int)] -> IntLP -> Bool
conditionHCl cl lvlM int
    | null areNotCons   = all isSmaller (clBodyDup cl)
    | otherwise         = any isSmaller areNotCons
    where
        isSmaller  = \x -> lvlMVal x lvlM < lvlMVal (clHead cl) lvlM
        areNotCons = intersect (clPBody cl) (faLP int) ++ intersect (clNBody cl) (trLP int)

conditionLP :: LP -> [(Atom, Int)] -> IntLP -> Bool
conditionLP lp lvlM int = all (\x -> conditionHCl x lvlM int) lp

conditionInts :: LP -> [(Atom, Int)] -> [IntLP] -> Bool
conditionInts lp lvlM ints = any (\x -> conditionLP lp lvlM x) ints

conditionLvlMs :: LP -> [[(Atom, Int)]] -> [IntLP] -> Bool
conditionLvlMs lp lvlMs ints = any (\x -> conditionInts lp x ints) lvlMs

isAcceptable :: LP -> Bool
isAcceptable lp = conditionLvlMs lp (possibleLvLMaps lp) (candidateInts lp)
