module FormulasL where

import Data.List


data Atom = A Int
            deriving Show

instance Eq Atom where
        A a == A b = a == b

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
hClBodyN (_, _, [])  = []
hClBodyN (_, _, ys) = ys

-- returns all heads of logic program
bPHead :: LogicP -> [Atom]              
bPHead []     = []
bPHead (x:xs) = hClHead x ++ bPHead xs

-- returns bodies of logic program
bPBody :: LogicP -> [Atom]              
bPBody []     = []
bPBody (x:xs) = hClBodyP x ++ hClBodyN x ++ bPBody xs

-- returns herbrand base of logic program (without duplicates)
bP :: LogicP -> [Atom]                 
bP [] = []
bP xs = nub (bPHead xs ++ bPBody xs)
