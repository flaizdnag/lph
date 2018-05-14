{-|
Module      : Completion
Description : Tools needed to perform Clark's completion for a logic program.
Copyright   : (c) Aleksandra Cz., 2017
                  Kinga O., 2017
                  Andrzej G., 2017
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Longer description
-}
module Completion
    ( Form (..)
    , Bool2 (..)
    , negP
    , negP'
    , logicP'
    , addN
    , atomsToForm
    , atomToForm
    , first
    , sorts
    , sortHeads
    , groupHeads
    , addC
    , sameHead
    , sameHead'
    , addDE1
    , addDE
    , compP
    , negA
    , compP'
    , alwaysTrue
    , groupByValue
    , breakForm
    , breakForms
    , inv
    , trueC
    , trueC'
    , trueD
    , trueD'
    , trueE
    , addHeadToInv
    , addNToForm
    , unForms
    , interps
    ) where

import Formulas
import Graph
import Operator
import Data.Graph
import Data.List
import GHC.Exts
import Examples


data Form = V Atom 
            | N Form        -- negation
            | C [Form]      -- conjunction 
            | D [Form]      -- disjunction 
            | E Form Form   -- equivalence
            | T             -- verum
                deriving (Show, Eq)

data Bool2 = Fa | Tr | Un
                deriving (Show, Eq)


negP :: LogicP -> [Atom]
negP []     = []
negP (x:xs) = nub (hClBodyN x ++ negP xs)

negP' :: LogicP -> [Atom]
negP' xs = nub (intToAtom [x | x <- atomToInt (bP xs), 
                               y <- atomToInt (negP xs), 
                               path g y x && not (path g x y)])
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
atomsToForm :: [Atom] -> [Form]
atomsToForm x = case x of
                    []     -> []
                    (y:ys) -> V y : atomsToForm ys

atomToForm :: Atom -> Form
atomToForm x = V x


-- returns first element of tuple with 3 elements
first :: (a, b, c) -> a
first (x, _, _) = x

-- ordering for HClauses in LogicP (by their head indexes)
sorts a b 
           | (first a) == (first b) = EQ
           | (first a) < (first b)  = LT
           | otherwise = GT

-- sorts HClauses by their heads indexes 
sortHeads :: LogicP -> LogicP
sortHeads xs = sortBy sorts xs

-- groups HClauses with the same heads in LogicP 
groupHeads :: LogicP -> [[HClause]]
groupHeads xs = groupBy (\z y -> ((sorts z y) == EQ)) (sortHeads xs)

-- connects elements of the horn clauses body by conjunction
addC :: HClause -> (Form, Form)
addC (h, [], []) = (V h, T)
addC (h, [], xs) 
                | length xs == 1 = (V h, N (V (head xs)))
                | otherwise      = (V h, C (addN xs))
addC (h, ys, []) 
                | length ys == 1 = (V h, V (head ys))
                | otherwise      = (V h, C (atomsToForm ys))
addC (h, ys, xs) = (V h, C (atomsToForm ys ++ addN xs))

sameHead :: [[HClause]] -> [[(Form, Form)]]
sameHead []     = []
sameHead (x:xs) = (map addC x) : sameHead xs

-- gives us list of lists with same head clauses as pairs 
-- (head, conjunction of body atoms)
sameHead' :: LogicP -> [[(Form, Form)]]
sameHead' [] = []
sameHead' xs = sameHead (groupHeads xs)

-- adds disjunction and equivalence to one group of clauses 
addDE1 :: [(Form, Form)] -> Form
addDE1 x 
        | (length x) > 1 = E (fst (head x)) (D (map snd x))
        | otherwise      = E (fst (head x)) (snd (head x))

-- maps adding disjunction and equivalence to whole LogicP
addDE :: LogicP -> [Form]
addDE [] = []
addDE xs = map addDE1 (sameHead' xs)

compP :: LogicP -> [Form]
compP xs = addDE xs ++ negA xs

-- adds negation to atoms in bodies that do not appear in heads
negA :: LogicP -> [Form]
negA [] = []
negA xs = addN ((bP xs) \\ (bPHead xs))

-- comp P-
compP' :: LogicP -> [Form]
compP' xs = compP (logicP' xs)

--checks if Formula is always True (T)
alwaysTrue :: Form -> Bool2
alwaysTrue x = case x of
                E _ T   -> Tr
                N _     -> Fa
                _       -> Un

-- groups Formulas by their values [[unknown], [True], [False]]
groupByValue :: [Form] -> [[Form]]
groupByValue xs = groupBy (\x y -> (alwaysTrue x) == (alwaysTrue y)) xs

--breaks complex formulas to variables
breakForm :: Form -> [Form]
breakForm a = case a of
                   V b   -> [V b]
                   N b   -> [b]
                   C b   -> b
                   D b   -> b
--                        case b of
--                                 [V d, xs] -> [V d] ++ breakForm xs
--                                 [N d, xs] -> [N d] ++ breakForm xs
--                                 [C d, xs] -> d ++ breakForm xs
                   E b c -> (breakForm b) ++ (breakForm c)
                   T     -> []

breakForms :: [Form] -> [Form]
breakForms []     = []
breakForms (x:xs) = breakForm x ++ breakForms xs

--generates interpretation from formulas with known value
inv :: [[Form]] -> ([Form], [Form])
inv xs = ([a | x <- xs, alwaysTrue (head x) == Tr, a <- (breakForms x)], [b | x <- xs, alwaysTrue (head x) == Fa, b <- (breakForms x)])

--checks if conjunction is True/False/Unknown
trueC :: Form -> ([Form], [Form]) -> [Bool2]
trueC (C []) _     = []
trueC (C (a:as)) x = case a of
                        N a -> if elem a (fst x) then Fa : (trueC (C as) x)
                               else 
                                   if elem a (snd x) then Tr : (trueC (C as) x) else Un : (trueC (C as) x)
                        a -> if elem a (snd x) then Fa : (trueC (C as) x)
                               else
                                   if elem a (fst x) then Tr : (trueC (C as) x) else Un : (trueC (C as) x)


trueC' :: Form -> ([Form], [Form]) -> Bool2
trueC' a x 
         | all (Tr==) (trueC a x) = Tr
         | any (Fa==) (trueC a x) = Fa
         | otherwise              = Un

--checks if disjunction is True/False/Unknown
trueD :: Form -> ([Form], [Form]) -> [Bool2]
trueD (D []) _ = []
trueD (D (a:as)) x  = case (a:as) of
                    (V b):bs -> if elem (V b) (fst x) then Tr : (trueD (D as) x)
                                else
                                    if elem (V b) (snd x) then Fa : (trueD (D as) x)
                                    else Un : (trueD (D as) x)
                    (N b):bs -> if elem b (snd x) then Tr : (trueD (D as) x) 
                                else
                                    if elem b (fst x) then Fa : (trueD (D as) x)
                                    else Un : (trueD (D as) x)
                    (C b):bs -> (trueC' (C b) x) : (trueD (D as) x)

trueD' :: Form -> ([Form], [Form]) -> Bool2
trueD' a x 
         | any (Tr==) (trueD a x) = Tr
         | all (Fa==) (trueD a x) = Fa
         | otherwise              = Un



-- checks if equivalence is True/False/Unknown
trueE :: Form -> ([Form], [Form]) -> Bool2
trueE (E a b) x = case b of 
                       V c -> if elem (V c) (fst x) then Tr 
                              else
                                  if elem (V c) (snd x) then Fa else Un 
                       N c -> if elem c (snd x) then Tr
                              else
                                  if elem c (fst x) then Fa else Un
                       C c -> trueC' (C c) x
                       D c -> trueD' (D c) x

-- adds heads from equivalences with known value
addHeadToInv :: Form -> ([Form], [Form]) -> ([Form], [Form])
addHeadToInv (E a b) (c, d) 
                    | ((elem a c) == False) && ((trueE (E a b) (c, d)) == Tr) = (a : c, d)
                    | ((elem a d) == False) && ((trueE (E a b) (c, d)) == Fa) = (c, a : d)
                    | otherwise                                               = (c, d) -- add from interps?


-- adds negation to formulas
addNToForm :: [Form] -> [Form]
addNToForm []     = []
addNToForm (x:xs) = N x : addNToForm xs

-- creates a list of all Forms from the LogicP that aren't included in I
unForms :: LogicP -> ([Form], [Form]) -> [Form]
unForms [] _ = []
unForms x y  = (a \\ (b ++ f)) ++ (c \\ (d ++ e)) 
                where 
                        a = nub (atomsToForm (bPHead x) ++ atomsToForm (bPBodyP x))
                        b = fst y
                        c = (addN (negP x))
                        d = addNToForm (snd y)
                        e = addNToForm (fst y) 
                        f = snd y

-- creates list of all permutations of Formulas we can add to I
interps :: LogicP -> ([Form], [Form]) -> [[Form]]
interps x y = sortWith length $ subsequences (unForms x y)

{-
funkcja2 ::  Form -> ([Form], [Form]) -> ([Form], [Form])
funkcja2 (E a b) x 
                | trueE' (E a b) x == Un = funkcja (E a b) x
                | trueE' (E a b) x == Fa = ([], [])
                | otherwise              = (zwraca interpretację)
-}
