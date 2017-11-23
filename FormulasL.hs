module Formulas where

data Atom = A Int | N Atom
            deriving (Show, Eq)

type HClause = (Atom, [Atom], [Atom])
type LogicP = [HClause]

hClHead :: HClause -> [Atom]            -- returns head of horn clause
hClHead (x, _, _) = [x]

hClBody :: HClause -> [Atom]            -- returns body of horn clause
hClBody (_, [], []) = []
hClBody (_, xs, ys) = xs ++ ys

hClBodyP :: HClause -> [Atom]           -- returns positive body of horn clause
hClBodyP (_, [], _) = []
hClBodyP (_, xs, _) = xs

hClBodyN :: HClause -> [Atom]           -- returns negative body of horn clause
hClBodyN (_, _, [])  = []
hClBodyN (_, _, ys) = ys

turnNtoP :: [Atom] -> [Atom]            -- changes sth negative to positive
turnNtoP []     = []
turnNtoP (x:xs) = case x of 
                        N a -> [a] ++ turnNtoP xs
                        _   -> [x] ++ turnNtoP xs

bPHead :: LogicP -> [Atom]              -- returns heads of logic program
bPHead []     = []
bPHead (x:xs) = case hClHead x of 
                        [N a] -> a : bPHead xs
                        _     -> hClHead x ++ bPHead xs

bPBody :: LogicP -> [Atom]              -- returns bodies of logic program
bPBody []     = []
bPBody (x:xs) = hClBodyP x ++ turnNtoP (hClBodyN x) ++ bPBody xs

rmdups :: Eq a => [a] -> [a]            -- removes duplicates
rmdups []     = []
rmdups (x:xs) = if   x `elem` xs 
                then rmdups xs 
                else x : rmdups xs

bP :: LogicP -> [Atom]                  -- returns herbrand base of logic program (without duplicates)
bP [] = []
bP xs = rmdups (bPHead xs ++ bPBody xs)