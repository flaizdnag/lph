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
    ) where

import Formulas
import Graph
import Completion
import Data.List

-- | The list of atoms that some atom occurring negatively in logic program
-- depends on.
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
alTomsLP lp as = map makeInt $ foldl1' redModels $ map modelsHCl $ getFa
    where
        getFa     = [ hcl | hcl <- lp, not $ evalHCl hcl (as, []) ]
        modelsHCl = \(h, _, nb) -> [ zs | x <- tail $ subsequences (h : nb), let zs = x ++ as ]
        redModels = \xs ys -> nub $ map redPairs $ allPairs xs ys
        redPairs  = \p -> sort $ nub $ fst p ++ snd p
        allPairs  = \xs ys -> [ (a, b) | a <- xs, b <- ys ]
        makeInt   = \xs -> alTointLP xs lp

alTointLP :: [Atom] -> LogicP -> IntLP
alTointLP as lp = (as, (bP lp) \\ as)

candidateInts :: LogicP -> [IntLP]
candidateInts lp = concatMap makeMsLP filtered
    where
        compP'      = comp (logicP' lp)
        filtered    = filter (\as -> comP'ms as) subseq
        subseq      = subsequences (negP lp)
        makeIntLP   = \xs -> alTointLP xs lp
        makeIntCPL  = \int -> intLPTointCPL int
        comP'ms     = \xs -> modelCheckCPL compP' $ makeIntCPL $ makeIntLP xs
        makeMsLP    = \xs -> alTomsLP lp xs
