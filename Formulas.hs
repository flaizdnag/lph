module Formulas where

import Data.List

{-
 - Logic programs in the propositional case are defined as sets of Horn
 - clauses, which in turn can be defined in the following way:
 -
 - a{0} <- a{1} , ... , a{n} , ~a{n+1} , ... a{m}
 -
 - where a{0}, ..., a{m} are propositions called atoms. Atom a{0} is called the
 - head of a Horn clause and the rest of the atoms, i.e. a{1}, ..., a{m} form
 - the body of a Horn clause. As it can be seen in the above example, there are
 - two types of atoms in the body of a Horn clause: atoms and negated atoms.
 - Since the negated atoms occur only in the body of a Horn clause, we decided
 - to instead of defining the negation itself, implement a Horn clause as a
 - tuple with atom (the head of a Horn clause), list of atoms that occur
 - without the negation in the body of a Horn clause and list of atoms that
 - occur with negation in the body of a Horn clause.
 -}

data Atom = A Int
            deriving Show

instance Eq Atom where
        A a == A b = a == b

instance Ord Atom where
        compare (A a) (A b) = compare a b

type HClause = (Atom, [Atom], [Atom])
type LogicP = [HClause]

-- returns head of horn clause
hClHead :: HClause -> [Atom]
hClHead (x, _, _) = [x]

-- returns body of horn clause
hClBody :: HClause -> [Atom]
hClBody (_, [], []) = []
hClBody (_, xs, ys) = xs ++ ys

-- returns positive body of horn clause
hClBodyP :: HClause -> [Atom]
hClBodyP (_, [], _) = []
hClBodyP (_, xs, _) = xs

-- returns negative body of horn clause
hClBodyN :: HClause -> [Atom]
hClBodyN (_, _, []) = []
hClBodyN (_, _, ys) = ys

-- returns all heads of logic program (w/o duplicates)
bPHead :: LogicP -> [Atom]              
bPHead []     = []
bPHead (x:xs) = nub (hClHead x ++ bPHead xs)

-- returns all heads of logic program (with duplicates)
bPHead' :: LogicP -> [Atom]              
bPHead' []     = []
bPHead' (x:xs) = (hClHead x) ++ (bPHead' xs)

-- returns bodies of logic program (w/o duplicates)
bPBody :: LogicP -> [Atom]              
bPBody []     = []
bPBody (x:xs) = nub (hClBodyP x ++ hClBodyN x ++ bPBody xs)
-- returns positive atoms from bodies of LogicP
bPBodyP :: LogicP -> [Atom]
bPBodyP []     = []
bPBodyP (x:xs) = nub (hClBodyP x ++ bPBodyP xs)

-- returns positive atoms from bodies of LogicP
bPBodyP :: LogicP -> [Atom]
bPBodyP []     = []
bPBodyP (x:xs) = nub (hClBodyP x ++ bPBodyP xs)

-- returns herbrand base of logic program (without duplicates)
bP :: LogicP -> [Atom]
bP [] = []
bP xs = nub (bPHead xs ++ bPBody xs)

