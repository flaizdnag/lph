module Completion where

import Formulas
import Graph
import Operator
import Data.Graph
import Data.List

data Form = V Atom 
            | N Form        -- negation
            | C [Form]      -- conjunction 
            | D [Form]      -- disjunction 
            | E Form Form   -- equivalence
            | T             -- Top symbol
            deriving Show

negP :: LogicP -> [Atom]
negP []     = []
negP (x:xs) = hClBodyN x ++ negP xs

negP' :: LogicP -> [Atom]
negP' xs = intToAtom [x | x <- atomToInt (bP xs), 
                          y <- atomToInt (negP xs), 
                          path g y x && not (path g x y)]
                          where g = graphG xs

-- creates a program which heads belong to negP'
logicP' :: LogicP -> LogicP
logicP' xs = [x | x <- xs, isElem (hClHead x) (negP' xs)]

-- CPL
-- changes negative atoms to N Form
addN :: [Atom] -> [Form]
addN x = case x of 
              []     -> []
              (y:ys) -> N (V y) : addN ys

-- changes positive atoms to V Atom
atomToForm :: [Atom] -> [Form]
atomToForm x = case x of
                    []     -> []
                    (y:ys) -> V y : atomToForm ys

-- connects elements of the horn clauses body by conjunction
addC :: HClause -> Form
addC (_, [], []) = T
addC (_, [], ys) 
                | length ys == 1 = N (V (head ys))
                | otherwise      = C (addN ys)
addC (_, xs, []) 
                | length xs == 1 = V (head xs)
                | otherwise      = C (atomToForm xs)
addC (_, xs, ys) = C (atomToForm xs ++ addN ys)

--addD

-- connects head and body of horn clause by equivalence
comp :: LogicP -> [Form]
comp []     = []
comp (x:xs) = case x of 
                 (h, _, _) -> (E (V h) (addC x)) : comp xs

