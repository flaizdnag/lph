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
atomsToForm :: [Atom] -> [Form]
atomsToForm x = case x of
                    []     -> []
                    (y:ys) -> V y : atomsToForm ys

atomToForm :: Atom -> Form
atomToForm x = V x

-- connects elements of the horn clauses body by conjunction
addC :: HClause -> Form
addC (_, [], []) = T

addC (_, [], (xs)) 
                 | length (xs) == 1 = N (V (head xs))
                 | otherwise        = C (addN (xs))
addC (_, (ys), []) 
                 | length (ys) == 1 = N (V (head ys))
                 | otherwise        = C (atomsToForm (ys))
addC (_, ys, xs) = C (atomsToForm ys ++ addN xs)


sameHead :: HClause -> (Form, Form)
sameHead x = (atomToForm (first x), addC x)

sameH1 :: [[HClause]] -> [[(Form, Form)]]
sameH1 []     = []
sameH1 (x:xs) = (map sameHead x) : sameH1 xs

--gives us list of lists with same head clauses as pairs (head, conjunction of body atoms)
sameH2 :: LogicP -> [[(Form, Form)]]
sameH2 [] = []
sameH2 xs = sameH1 (groupHeads xs)

--adds disjunction to one group of clauses (with the same head)
addD :: [(Form, Form)] -> (Form, Form)
addD x 
            | (length x) > 1 = (fst (head x), D (map snd x))
            | otherwise       = head x

--maps adding disjunction to whole LogicP
addD1 :: LogicP -> [(Form, Form)]
addD1 [] = []
addD1 xs = map addD (sameH2 xs)


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

-- connects head and body of horn clause by equivalence
comp :: LogicP -> [Form]
comp []     = []
comp (x:xs) = case x of 
                 (h, _, _) -> (E (V h) (addC x)) : comp xs

-- LEVEL MAPPING

-- generates list of subsequent numbers as long as given list
numList :: [Atom] -> [Int]
numList [] = []
numList xs = [x |x <- [1..n]]
                where
                    n = (length xs)

-- checks if head does not appear in bodies
onlyHead :: LogicP -> Atom -> Bool
onlyHead [] _      = False
onlyHead xs y = if elem y (bPBody xs) 
                        then False
                        else True

-- checks if atom appears only in bodies
onlyBody :: LogicP -> Atom -> Bool
onlyBody [] _      = False
onlyBody xs y = if elem y (bPHead xs) 
                        then False
                        else True

-- sorts Bp elements into 3 lists in order: 
-- elements that appear only in heads,
-- elements that appear both in heads and bodies
-- elements that appear only in bodies.
sortElems :: LogicP -> ([Atom], [Atom], [Atom])
sortElems xs = ([x | x <- (bPHead xs), onlyHead xs x], 
                [y | y <- (bPHead xs), (onlyHead xs y) == False], 
                [z | z <- (bPBody xs), onlyBody xs z])


-- replaces atoms with assigned numbers
mapNum :: ([Atom], [Atom], [Atom]) -> [Int] -> ([Int], [Int], [Int])
mapNum (a, b, c) xs = (x, y, z)
                            where
                                z = (take (length c) xs)
                                x = (drop ((length xs) - (length a)) xs)
                                y = (xs \\ (x ++ z))

mapNum' :: LogicP -> ([Int], [Int], [Int])
mapNum' xs = mapNum (sortElems xs) (numList (bP xs))

-- returns permutations of list
perms :: ([Atom], [Atom], [Atom]) -> [[Atom]]
perms (a, b, c) = permutations b

--checker??

-- replaces middle list with one of the permutations (depended on checker)
replaceNum :: ([Atom], [Atom], [Atom]) -> [[Atom]] -> ([Atom], [Atom], [Atom])
replaceNum (a, b, c) (x:xs) = (a, x, c)

{-lvlMap (x:xs) = if checker (numMap (a, x, c)) 
                    then numMap (a, x, c) 
                    else lvlMap xs
-}