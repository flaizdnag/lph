{-|
Module      : Formulas2
Description : Logic programs with basic properties as Herbrand base or parts of
              Horn clauses.
Copyright   : (c) Aleksandra Cz., 2017--2018
                  Kinga O., 2017--2018
                  Andrzej G., 2017--2018
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Formulas2
    ( L_Atom (..)
    , L_HClause
    , L_LogicP
    , f
    , labelAtom
    , labelNegAtom
    , addlabeltoHClAtom
    , labelBodyP
    , labelBodyN
    , dn
    , hCltoFacts
    , logicPtoPp
    , logicPtoPhi
    ) where

import Data.List
import Formulas

data L_Atom = A_ Int [Char]
              deriving Show

-- not sure which labels are equal
instance Eq L_Atom where
  A_ a _ == A_ b _ = a == b
--  A_ a x == A_ b y = a == b && x == y 
--  A_ a x == A_ b y = a == b && (elem x [y] || elem y [x]) -- sth's not working here

type L_HClause = (L_Atom, [L_Atom], [L_Atom])

type L_LogicP = [L_HClause]

f :: Atom -> Int
f (A int) = int

labelAtom :: Atom -> L_Atom
labelAtom a = A_ (f a) []

labelNegAtom :: Atom -> L_Atom
labelNegAtom a = A_ (f a) ['n']

addlabeltoHClAtom :: L_Atom -> L_Atom
addlabeltoHClAtom (A_ int label) = A_ int ('h' : label)

labelBodyP :: [Atom] -> [L_Atom]
labelBodyP = map labelAtom

labelBodyN :: [Atom] -> [L_Atom]
labelBodyN = map labelNegAtom

dn :: HClause -> L_HClause
dn (h, pos, neg) = (labelAtom h, labelBodyP pos, labelBodyN neg)

hCltoFacts :: HClause -> [L_HClause]
hCltoFacts (_, [], [])     = []
hCltoFacts (h, p:pos, neg) = (addlabeltoHClAtom (labelAtom p), [], []) : hCltoFacts (h, pos, neg)
hCltoFacts (h, [], n:neg)  = (addlabeltoHClAtom (labelNegAtom n), [], []) : hCltoFacts (h, [], neg)

logicPtoPp :: LogicP -> L_LogicP
logicPtoPp = map dn

logicPtoPhi :: LogicP -> HClause -> L_LogicP
logicPtoPhi xs h = logicPtoPp xs ++ hCltoFacts h