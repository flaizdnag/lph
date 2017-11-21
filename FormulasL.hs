module Formulas where

data Atom = A Int | N Atom
            deriving (Show)

type HClause = (Atom, [Atom], [Atom])
type LogicP = [HClause]

hClBody :: HClause -> [Atom]
hClBody (_, [],[]) = []
hClBody (_, xs, ys) = xs ++ ys


hClHead :: HClause -> [Atom]
hClHead (x, _,_) = [x]


hClBodyP :: HClause -> [Atom]
hClBodyP (_, [],_)    = []
hClBodyP (_, xs, _)   = xs 


hClBodyN :: HClause -> [Atom]
hClBodyN (_, _,[])   = []
hClBodyN (_, _, ys)   = ys

bPHead :: LogicP -> [Atom]
bPHead []     = []
bPHead (x:xs) = hClHead x ++ bPHead xs 

bPBody :: LogicP -> [Atom] --dodać usuwanie negacji
bPBody [] = []
bPBody (x:xs) = hClBody x ++ bPBody xs

bP :: LogicP -> [Atom]
bP xs = bPHead xs ++ bPBody xs

{- 
show:   atoms: A Int -> a Int
        literals: a Int, n a Int
        horn clauses: a Int <- a Int, a Int, ...
        logic program: horn clause, ...


immediate consequence operator 
Tp :: LogicP -> [Atom] -> [Atom] 
                 ^ tu nie ma zanegowanych ciałek
                           ^ zwraca listę głów
(funkcja, która będzie sprawdzać czy lista jest podlistą drugiej listy  -- any?)

-}