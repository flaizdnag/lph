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

-- | Creates a program from the initial LP whose heads belong to negP.
logicP' :: LogicP -> LogicP
logicP' lp = [ hcl | hcl <- lp, elem (hClHead hcl) (negP lp) ]
