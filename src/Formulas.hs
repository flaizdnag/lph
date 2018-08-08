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
    , IntLP
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
    , onlyHeads
    , bPBodiesDup
    , bPBodies
    , onlyBodies
    , bPBodiesPDup
    , bPBodiesP
    , bPBodiesNDup
    , bPBodiesN
    , bPDup
    , bP 
    , atomDef
    , evalAtomLP
    , evalHCl
    , modelCheckLP
    ) where

import Auxiliary
import Data.List

-- | Atoms are basic structures for Horn clauses.
data Atom = A Int [Char]
    deriving (Show, Read)

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

-- | An interpretation is a tuple with lists of atoms: the first list contains
-- atoms that are mapped to 'truth' and the second those that are mapped to
-- 'false'.
type IntLP = ([Atom], [Atom])

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

-- | Function that returns a list of atoms that are heads of some Horn clauses
-- from a given logic program, and in the same time they do not occur in any
-- body of those Horn clauses.
onlyHeads :: LogicP -> [Atom]
onlyHeads lp = [ a | a <- bPHeads lp, not $ elem a (bPBodies lp) ]

-- | Function that returns atoms from the bodies of all Horn clauses from
-- a given logic program with duplicates.
bPBodiesDup :: LogicP -> [Atom]
bPBodiesDup = concatMap hClBodyDup

-- | Function that returns atoms from the bodies of all Horn clauses from
-- a given logic program without duplicates.
bPBodies :: LogicP -> [Atom]
bPBodies = nub . bPBodiesDup

-- | Function that returns a list of atoms that occur in a body of some Horn
-- clause from a given logic program, and in the same time they do not occur in
-- any head of those Horn clauses.
onlyBodies lp = [ a | a <- bPBodies lp, not $ elem a (bPHeads lp) ]

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

--------------------------------------------------------------------------------
-- Checking if a given interpretation is a model for atom,Horn clause, logic  --
-- program                                                                    --
--------------------------------------------------------------------------------

-- | Takes an atom and an interpretation, and returns the value of the
-- evaluation of the atom, i.e. 'true' or 'false'. The assumption here is that
-- every atom has a value, therefore, if an atom does not belong to the first
-- set of an interpretation (the set with 'true' atoms), then it belongs to the
-- second list of the interpretation (the set with 'false' atoms).
evalAtomLP :: Atom -> IntLP -> Bool
evalAtomLP (A i lab) (tr, fa)
    | elem (A i lab) tr = True
    | otherwise         = False

-- | Takes a Horn clause and an interpretation, and returns the value of the
-- evaluation of the Horn clause.
evalHCl :: HClause -> IntLP -> Bool
evalHCl (h, pb, nb) (tr, _)
    | elem h tr              = True
    | jointElem nb tr        = True
    | not $ isSublist pb tr  = True
    | otherwise              = False

-- | Checks if a given interpretation is a model for a given logic program.
modelCheckLP :: LogicP -> IntLP -> Bool
modelCheckLP lp int = all (\x -> evalHCl x int) lp
