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

where \(a_0\), ..., \(a_m\) are propositions called atoms, \(\leftarrow\) is a
simple connector and \(\sim\) represents negation as finite failure.
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
    , hClHead
    , hClBody
    , hClBodyP
    , hClBodyN
    , bPHead
    , bPBody
    , bPBodyP
    , bP 
    ) where

import Data.List

-- | Atoms are basic structures for Horn clauses.
data Atom = A Int
            deriving Show

instance Eq Atom where
        A a == A b = a == b

instance Ord Atom where
        compare (A a) (A b) = compare a b

-- | We do not implement negation as finite failure --- instead, the negated
-- atoms from the body of a Horn clause are kept in a separate list. The tuple
-- is organised in the following way: atom, which is the head of a Horn clause,
-- list of atoms from the body of a Horn clause that are not preceded by
-- negation, list of atoms from the body of a Horn clause that are negated.
type HClause = (Atom, [Atom], [Atom])

-- | Logic program is a list of Horn clauses.
type LogicP = [HClause]

-- | Function that returns the head of a Horn clause.
hClHead :: HClause -> [Atom]
hClHead (x, _, _) = [x]

-- | Function that returns atoms from the body of a Horn clause (atomic body).
hClBody :: HClause -> [Atom]
hClBody (_, xs, ys) = nub (xs ++ ys)

-- | Function that returns atoms from the body of a Horn clause that are not
-- preceded by negation (positive body).
hClBodyP :: HClause -> [Atom]
hClBodyP (_, xs, _) = xs

-- | Function that returns atoms from the body of a Horn clause that are
-- preceded by negation (negative body).
hClBodyN :: HClause -> [Atom]
hClBodyN (_, _, ys) = ys

-- | Function that returns all heads of Horn clauses from a given logic program
-- --- without duplicates.
bPHead :: LogicP -> [Atom]              
bPHead []     = []
bPHead (x:xs) = nub (hClHead x ++ bPHead xs)

-- | Function that returns atoms from the bodies of all Horn clauses from a
-- given logic program --- without duplicates.
bPBody :: LogicP -> [Atom]              
bPBody []     = []
bPBody (x:xs) = nub (hClBodyP x ++ hClBodyN x ++ bPBody xs)

-- | Function that returns positive atoms from bodies of all Horn clauses form a
-- given logic program --- without duplicates.
bPBodyP :: LogicP -> [Atom]
bPBodyP []     = []
bPBodyP (x:xs) = nub (hClBodyP x ++ bPBodyP xs)

-- | Function that returns the Herbrand base of a logic program, i.e. the list
-- of all atoms that occur in the Horn clauses from a given logic program ---
-- without duplicates.
bP :: LogicP -> [Atom]
bP [] = []
bP xs = nub (bPHead xs ++ bPBody xs)
