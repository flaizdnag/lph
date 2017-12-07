module CCompletion where

import FormulasL
import GraphL
import Operator
<<<<<<< HEAD
import Data.Graph

data Form =   V Atom 
            | N Form        -- negation
            | C [Form]      -- conjunction - C Form Form? 
            | D [Form]      -- disjunction - D Form Form? 
            | E Form Form   -- equivalence
            deriving Show
=======
import CPL
import Data.Graph
>>>>>>> bd46f1deff13b25326fcc8e85132a3552b75055f

data Form =   V Atom 
            | N Form        -- negation
            | C [Form]      -- conjunction - C Form Form? 
            | D [Form]      -- disjunction - D Form Form? 
            | E Form Form   -- equivalence
            deriving Show
            
negP :: LogicP -> [Atom]
negP []     = []
negP (x:xs) = hClBodyN x ++ negP xs

negP' :: LogicP -> [Atom]
negP' xs = intToAtom [x | x <- atomToInt (bP xs), 
                          y <- atomToInt (negP xs), 
                          path g y x && not (path g x y)]
<<<<<<< HEAD
                          where g = graphG xs
=======
                          where g = $ graphG xs
>>>>>>> bd46f1deff13b25326fcc8e85132a3552b75055f

-- creates a program which heads belong to negP'
logicP' :: LogicP -> LogicP
logicP' xs = [x | x <- xs, isElem (hClHead x) (negP' xs)]

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
bodyToC :: HClause -> Form
bodyToC x = case x of 
{-
                (_, [], neg)  -> case neg of
                                      n -> N (V n)
                                      (n:ns) -> C (addN (n:ns))
                (_, pos, [])  -> case pos of
                                      p -> V p
                                      (p:ps) -> C (atomToForm (p:ps)) ??? -}
                (_, pos, neg) -> C (atomToForm pos ++ addN neg)

-- connects head and body of horn clause by equivalence
hClToEq :: HClause -> Form
hClToEq x = case x of 
                 (h, _, _) -> E (V h) (bodyToC x) 

comp :: LogicP -> [Form]
comp []     = []
<<<<<<< HEAD
comp (x:xs) = hClToEq x : comp xs
=======
comp (x:xs) = hClToEq x : comp xs
>>>>>>> bd46f1deff13b25326fcc8e85132a3552b75055f
