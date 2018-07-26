{-|
Module      : Formulas
Description : Logic programs with basic properties as Herbrand base or parts of
              Horn clauses.
Copyright   : (c) Aleksandra Cz., 2017--2018
                  Kinga O., 2017--2018
                  Andrzej G., 2017--2018
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Logic programs in the propositional case are defined as sets of Horn clauses,
which in turn can be defined in the following way:

\[
a_0 \leftarrow a_1 , ... , a_n , \sim\!a_{n+1} , ... , \sim\!a_m
\]

where \(a_0\), ..., \(a_m\) are propositions called atoms, \(\leftarrow\) is
a simple connector and \(\sim\) represents negation as finite failure.
Atom \(a_0\) is called the head of a Horn clause and the rest of the atoms,
i.e. \(a_1\), ..., \(a_m\), form the body of a Horn clause. As it can be seen in
the example above, there are two types of literals in the body of a Horn clause:
atoms and negated atoms. Since the negated atoms occur only in the body of a
Horn clause, we decided to implement a Horn clause as a tuple with atom (the
head of a Horn clause), a list of atoms that occur without the negation in the
body of a Horn clause, and a list of atoms that occur with negation in the body
of a Horn clause, instead of defining the negation itself.

This module contains definitions of types for atoms, Horn clauses and logic
programs, as well as functions that provide essential properties of Horn clauses
and logic programs.
-}
module Formulas
    ( Atom (..)
    , HClause
    , LogicP
    , atomIdx
    , atomLab
    , hClHead
    , hClBodyDup
    , hClBody
    , hClBodyPDup
    , hClBodyP
    , hClBodyNDup
    , hClBodyN
    , bPHeadsDup
    , bPHeads
    , bPBodiesDup
    , bPBodies
    , bPBodiesPDup
    , bPBodiesP
    , bPBodiesNDup
    , bPBodiesN
    , bPDup
    , bP 
    , atomDef
    ) where

import Auxiliary
import Data.List

-- | Atoms are basic structures for Horn clauses.
data Atom = A Int [Char]
    deriving Show

instance Eq Atom where
    A a xs == A b ys = a == b && eqLists xs ys

instance Ord Atom where
    A a xs < A b ys = a < b || (a == b && ltLists xs ys)
    
    a <= b = (a < b) || (a == b)
    a >  b = b < a
    a >= b = b <= a

-- | We do not implement negation as finite failure --- instead, the negated
-- atoms from the body of a Horn clause are kept in a separate list. The tuple
-- is organised in the following way: atom, which is the head of a Horn clause,
-- list of atoms from the body of a Horn clause that are not preceded by
-- negation, list of atoms from the body of a Horn clause that are negated.
type HClause = (Atom, [Atom], [Atom])

-- | Logic program is a list of Horn clauses.
type LogicP = [HClause]

-- | Returns the index of an atom.
atomIdx :: Atom -> Int
atomIdx (A n _) = n

-- | Returns the label of an atom.
atomLab :: Atom -> [Char]
atomLab (A _ lab) = lab

-- | Function that returns the head of a Horn clause.
hClHead :: HClause -> Atom
hClHead = first

-- | Function that returns atoms from the body of a Horn clause (atomic body)
-- with duplicates.
hClBodyDup :: HClause -> [Atom]
hClBodyDup a = second a ++ third a
--
-- | Function that returns atoms from the body of a Horn clause (atomic body)
-- without duplicates.
hClBody :: HClause -> [Atom]
hClBody = nub . hClBodyDup

-- | Function that returns atoms from the body of a Horn clause that are not
-- preceded by negation (positive body) with duplicates.
hClBodyPDup :: HClause -> [Atom]
hClBodyPDup = second

-- | Function that returns atoms from the body of a Horn clause that are not
-- preceded by negation (positive body) without duplicates.
hClBodyP :: HClause -> [Atom]
hClBodyP = nub . hClBodyPDup

-- | Function that returns atoms from the body of a Horn clause that are
-- preceded by negation (negative body) with duplicates.
hClBodyNDup :: HClause -> [Atom]
hClBodyNDup = third

-- | Function that returns atoms from the body of a Horn clause that are
-- preceded by negation (negative body) without duplicates.
hClBodyN :: HClause -> [Atom]
hClBodyN = nub . hClBodyNDup

-- | Function that returns all heads of Horn clauses from a given logic program
-- with duplicates.
bPHeadsDup :: LogicP -> [Atom]
bPHeadsDup = map (first)

-- | Function that returns all heads of Horn clauses from a given logic program
-- without duplicates.
bPHeads :: LogicP -> [Atom]
bPHeads = nub . bPHeadsDup

-- | Function that returns atoms from the bodies of all Horn clauses from
-- a given logic program with duplicates.
bPBodiesDup :: LogicP -> [Atom]
bPBodiesDup = concatMap hClBodyDup

-- | Function that returns atoms from the bodies of all Horn clauses from
-- a given logic program without duplicates.
bPBodies :: LogicP -> [Atom]
bPBodies = nub . bPBodiesDup

-- | Function that returns positive atoms from bodies of all Horn clauses form
-- a given logic program with duplicates.
bPBodiesPDup :: LogicP -> [Atom]
bPBodiesPDup = concatMap hClBodyPDup

-- | Function that returns positive atoms from bodies of all Horn clauses form
-- a given logic program without duplicates.
bPBodiesP :: LogicP -> [Atom]
bPBodiesP = nub . bPBodiesPDup

-- | Function that returns negative atoms from bodies of all Horn clauses form
-- a given logic program with duplicates.
bPBodiesNDup :: LogicP -> [Atom]
bPBodiesNDup = concatMap hClBodyNDup

-- | Function that returns negative atoms from bodies of all Horn clauses form
-- a given logic program with duplicates.
bPBodiesN :: LogicP -> [Atom]
bPBodiesN = nub . bPBodiesNDup

-- | Function that returns the Herbrand base of a logic program, i.e. the list
-- of all atoms that occur in the Horn clauses from a given logic program with
-- duplicates.
bPDup :: LogicP -> [Atom]
bPDup lp = bPHeadsDup lp ++ bPBodiesDup lp

-- | Function that returns the Herbrand base of a logic program, i.e. the list
-- of all atoms that occur in the Horn clauses from a given logic program
-- without duplicates.
bP :: LogicP -> [Atom]
bP = nub . bPDup

-- | Definition of an atom is the set of all Horn clauses that have the atom as
-- the head.
atomDef :: Atom -> LogicP -> LogicP
atomDef a lp = [ hcl | hcl <- lp, hClHead hcl == a ]
