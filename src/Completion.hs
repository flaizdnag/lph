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
    , atomToVar
    , first
    , sorts
    , sortHeads
    , groupHeads
    , addC
    , sameHead
    , sameHead'
    , addDE'
    , addDE
    , compP
    , negA
    , compP'
    , invariants
    , groupByValue
    , breakForm
    , breakForms
    , interp
    , unE
    , trueC
    , trueC'
    , trueD
    , trueD'
    , trueE
    , addHeadToI
    , accI
    , addNToForm
    , perms
    , unAtoms
    , iterI
    , checkUn
    , check
    , check'
    ) where

import Formulas
import Graph
import Operator
import Auxiliary
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


-- list of all negative literals from a logic program
negP :: LogicP -> [Atom]
negP []     = []
negP (x:xs) = nub (hClBodyN x ++ negP xs)

-- list of atoms that atoms from negP depend on, i.e. there is a path between
-- atoms a_i and a_j, where a_i is an element form negP
--
-- TODO sthing is wrong with the 'path' function: it always returns 'true' for
-- paths between a node and itself
negP' :: LogicP -> [Atom]
negP' xs = nub (intToAtom [x | x <- atomToInt (bP xs),
                               y <- atomToInt (negP xs),
                               path g x y])
                               where g = graph xs

-- creates a program from the initial LP whose heads belong to negP'
logicP' :: LogicP -> LogicP
logicP' xs = [x | x <- xs, isElem (hClHead x) (negP' xs)]

-- CPL
-- changes a list of atoms to negated variables
addN :: [Atom] -> [Form]
addN x = case x of 
              []     -> []
              (y:ys) -> N (V y) : addN ys

atomToNVar :: Atom -> Form
atomToNVar x = N (V x)

atomsToNVar :: [Atom] -> [Form]
atomsToNVar = map atomToNVar

-- changes a list of atoms to variables
atomsToForm :: [Atom] -> [Form]
atomsToForm x = case x of
                    []     -> []
                    (y:ys) -> V y : atomsToForm ys

atomToVar :: Atom -> Form
atomToVar x = V x

atomsToVar :: [Atom] -> [Form]
atomsToVar = map atomToVar


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
sortHeads = sortBy sorts

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
addDE' :: [(Form, Form)] -> Form
addDE' x 
        | (length x) > 1 = E (fst (head x)) (D (map snd x))
        | otherwise      = E (fst (head x)) (snd (head x))

-- maps adding disjunction and equivalence to whole LogicP
addDE :: LogicP -> [Form]
addDE [] = []
addDE xs = map addDE' (sameHead' xs)

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
invariants :: Form -> Bool2
invariants x = case x of
                E _ T   -> Tr
                N _     -> Fa
                _       -> Un

-- groups Formulas by their values [[Unknown], [True], [False]]
groupByValue :: [Form] -> [[Form]]
groupByValue xs = groupBy (\x y -> (invariants x) == (invariants y)) xs

-- breaks complex formulas to variables
-- TODO problems with Disjunction and Equivalence, e.g.
-- breakForm (D [N (V (A1))]) = [N (V (A 1))]
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

--generates interp from formulas with known value
interp :: LogicP -> ([Form], [Form])
interp xs = (true, false)
    where
        true  = [a | x <- (groupByValue (compP xs)), invariants (head x) == Tr, a <- (breakForms x)]
        false = [b | x <- (groupByValue (compP xs)), invariants (head x) == Fa, b <- (breakForms x)]

-- returns list of formulas with unknown value
-- TODO Why not use 'groupByValue'?
unE :: [Form] -> [Form]
unE []                        = []
unE (x:xs) 
         | invariants x == Un = x: unE xs
         | otherwise          = unE xs

--checks if conjunction is True/False/Unknown
trueC :: Form -> ([Form], [Form]) -> [Bool2]
trueC (C []) _     = []
trueC (C (a:as)) x = case a of
    N a -> if elem a (fst x) then Fa : (trueC (C as) x)
           else 
               if elem a (snd x) then Tr : (trueC (C as) x)
               else Un : (trueC (C as) x)
    a   -> if elem a (snd x) then Fa : (trueC (C as) x)
           else
               if elem a (fst x) then Tr : (trueC (C as) x)
               else Un : (trueC (C as) x)


trueC' :: Form -> ([Form], [Form]) -> Bool2
trueC' a x 
         | all (Tr==) (trueC a x) = Tr
         | any (Fa==) (trueC a x) = Fa
         | otherwise              = Un

-- checks if disjunction is True/False/Unknown
-- TODO What with 'T'? Cases have to be based on the whole list, i.e. 'a:as'?
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
addHeadToI :: Form -> ([Form], [Form]) -> ([Form], [Form])
addHeadToI (E a b) (c, d) 
    | (not (elem a c)) && ((trueE (E a b) (c, d)) == Tr) = (a : c, d)
    | (not (elem a d)) && ((trueE (E a b) (c, d)) == Fa) = (c, a : d)
    | otherwise                                          = (c, d) -- add from perms

-- takes 3 arguments:
-- -list of equivalences with unknown values
-- -list with permutations of all atoms unused in interpretation 
-- -current interpretation
-- it checks if with current interpretation we can decide values 
-- of other atoms, in result returns actualized interpretation
accI :: [Form] -> [[Form]] -> ([Form], [Form]) -> ([Form], [Form])
accI [] _ (a, b) = (a, b)
accI _ [] (a, b) = (a, b)                
accI (x:xs) (y:ys) (a, b) 
    | (trueE x ((y ++ a), b)) == Un = accI (xs ++ [x]) (y:ys) (a, b)
    | otherwise                     = accI xs (ys) (addHeadToI x (a, b))

-- generalizes type of accInv to make it easier to use in other functions
iterI :: LogicP -> ([Form], [Form])
iterI x = accI (unE (compP x)) (perms x (interp x)) (interp x)

-- checks if generated interpretation lets us establish value of
-- chosen equivalence categorized earlier as unknown
checkUn :: LogicP -> Int -> Bool2
checkUn x n = (trueE (unE (compP x) !! n) (iterI x))

-- gives a list of the values for all the equivalences 
-- that we are able to establish with generated interpretation
check :: LogicP -> Int -> [Bool2]
check x (-1) = []
check x n = (checkUn x n) : check x (n-1)

-- checks if list given by function check contains any
-- 'Un' value
-- if not - returns generated interpretation
-- if it does - returns empty interpretation
check' :: LogicP -> ([Form], [Form])
check' x = if (notElem Un (check x n)) then iterI x
                else ([],[])
                   where
                       n = (length (unE (compP x))) - 1

-- adds negation to formulas
addNToForm :: [Form] -> [Form]
addNToForm []     = []
addNToForm (x:xs) = N x : addNToForm xs

-- creates a list of all Forms from the LogicP that aren't included in I
unAtoms :: LogicP -> ([Form], [Form]) -> [Form]
unAtoms [] _ = []
unAtoms x y  = (a \\ (b ++ f)) ++ (c \\ (d ++ e)) 
                where 
                        a = nub (atomsToForm (bPHead x) ++ atomsToForm (bPBodyP x))
                        b = fst y
                        c = (addN (negP x))
                        d = addNToForm (snd y)
                        e = addNToForm (fst y) 
                        f = snd y

-- creates list of all permutations of Formulas we can add to I
perms :: LogicP -> ([Form], [Form]) -> [[Form]]
perms x y = sortWith length $ subsequences (unAtoms x y)
