module Completion where

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
isTrue :: Form -> Bool
isTrue x = case x of
                E _ T -> True
                _     -> False

-- groups Formulas by their values [[unknown], [True], [False]]
groupByValue :: [Form] -> [[Form]]
groupByValue xs = groupBy (\x y -> (isTrue x) == (isTrue y)) xs

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

-- invariants in possible interpretation ([True], [False])
inv :: [[Form]] -> ([Form], [Form])
inv xs = ((breakForms (xs !! 1)), (breakForms (xs !! 2)))

-- checking if conjunction is True/False/Unknown
trueC :: Form -> ([Form], [Form]) -> Bool2
trueC (C []) _     = Tr
trueC (C (a:as)) x = case a of
                         N a -> if elem a (fst x) then Fa
                                else 
                                    if elem a (snd x) then trueC (C as) x else Un
                         a   -> if elem a (snd x) then Fa
                                else            
                                    if elem a (fst x) then trueC (C as) x else Un

-- checking if disjunction is True/False/Unknown
trueD :: Form -> ([Form], [Form]) -> Bool2
trueD (D []) _ = Fa
trueD (D a) x  = case a of
                    (V b):bs -> if elem (V b) (fst x) then Tr else trueD (D bs) x
                    (N b):bs -> if elem b (snd x) then Tr else trueD (D bs) x
                    (C b):bs -> if ((trueC (C b) x) == Tr) then Tr else trueD (D bs) x

trueD' :: Form -> ([Form], [Form]) -> Bool2
trueD' (D []) _ = Fa
trueD' (D a) x  = case a of
                    (V b):bs -> if (elem (V b) (snd x)) == False then Un else trueD' (D bs) x
                    (N b):bs -> if (elem b (fst x)) == False then Un else trueD (D bs) x
                    (C b):bs -> if ((trueC (C b) x) == Un) then Un else trueD (D bs) x

trueD'' :: Form -> ([Form], [Form]) -> Bool2
trueD'' a b 
            | (trueD a b) == Tr = Tr
            | otherwise         = trueD' a b

-- checks if equivalence is True/False/Unknown
trueE :: Form -> ([Form], [Form]) -> Bool2
trueE (E a b) x = case b of 
                       V c -> if elem (V c) (fst x) then Tr else Un
                       N c -> if elem c (snd x) then Tr else Un
                       C c -> trueC (C c) x
                       D c -> trueD'' (D c) x

trueE' :: Form -> ([Form], [Form]) -> Bool2
trueE' (E a b) (c, d) 
                    | (elem a c) && ((trueE (E a b) (c, d)) == Tr)            = Tr
                    | ((elem a c) == False) && ((trueE (E a b) (c, d)) == Fa) = Fa
                    | otherwise                                               = Un

-- adds negation to formulas
addNToForm :: [Form] -> [Form]
addNToForm []     = []
addNToForm (x:xs) = N x : addNToForm xs

-- creates a list of all Forms from the LogicP that aren't included in I
allForms :: LogicP -> ([Form], [Form]) -> [Form]
allForms [] _ = []
allForms x y  = (a \\ (b ++ f)) ++ (c \\ (d ++ e)) 
                where 
                        a = nub (atomsToForm (bPHead x) ++ atomsToForm (bPBodyP x))
                        b = fst y
                        c = (addN (negP x))
                        d = addNToForm (snd y)
                        e = addNToForm (fst y) 
                        f = snd y

-- creates list of all permutations of Formulas we can add to I
interps :: LogicP -> ([Form], [Form]) -> [[Form]]
interps x y = sortWith length $ subsequences (allForms x y)

{-
funkcja :: Form -> ([Form], [Form]) -> ([Form], [Form])
funkcja (E a b) (c, d) 
                | trueE (E a b) (c, d) == Tr = (a:c, d)
                | trueE (E a b) (c, d) == Fa = (c, a:d)
                | otherwise                  = interps

funkcja2 ::  Form -> ([Form], [Form]) -> ([Form], [Form])
funkcja2 (E a b) x 
                | trueE' (E a b) x == Un = funkcja (E a b) x
                | trueE' (E a b) x == Fa = ([], [])
                | otherwise              = (zwraca interpretację)
-}

-- LEVEL MAPPING

-- generates list of subsequent numbers as long as given list
numList :: [Atom] -> [Int]
numList [] = []
numList xs = [x | x <- [1..n]]
             where n = length xs

-- checks if head does not appear in bodies
onlyHead :: LogicP -> Atom -> Bool
onlyHead [] _ = False
onlyHead xs y = if elem y (bPBody xs) then False
                else True

-- checks if atom appears only in bodies
onlyBody :: LogicP -> Atom -> Bool
onlyBody [] _ = False
onlyBody xs y = if elem y (bPHead xs) then False
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

{-
lvlMap (x:xs) = if checker (mapNum (a, x, c)) 
                then mapNum (a, x, c) 
                else lvlMap xs
-}