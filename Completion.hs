module Completion where

import Formulas
import Graph
import Operator
import Data.Graph
import Data.List
import Examples

data Form = V Atom 
            | N Form        -- negation
            | C [Form]      -- conjunction 
            | D [Form]      -- disjunction 
            | E Form Form   -- equivalence
            | T             -- verum
            | U             -- undecided
            deriving Show

instance Eq Form where
         V a == V b = a == b
         N a == N b = a == b

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
addC :: HClause -> (Form, Form)
addC (h, [], []) = (V h, T)
addC (h, [], xs) 
                | length xs == 1 = (V h, N (V (head xs)))
                | otherwise      = (V h, C (addN xs))
addC (h, ys, []) 
                | length ys == 1 = (V h, V (head ys))
                | otherwise      = (V h, C (atomsToForm ys))
addC (h, ys, xs) = (V h, C (atomsToForm ys ++ addN xs))

--sameHead :: HClause -> (Form, Form)
--sameHead x = (atomToForm (first x), addC x)

sameH1 :: [[HClause]] -> [[(Form, Form)]]
sameH1 []     = []
sameH1 (x:xs) = (map addC x) : sameH1 xs

-- gives us list of lists with same head clauses as pairs 
-- (head, conjunction of body atoms)
sameH2 :: LogicP -> [[(Form, Form)]]
sameH2 [] = []
sameH2 xs = sameH1 (groupHeads xs)

-- adds disjunction and equivalence to one group of clauses 
addDE1 :: [(Form, Form)] -> Form
addDE1 x 
        | (length x) > 1 = E (fst (head x)) (D (map snd x))
        | otherwise      = E (fst (head x)) (snd (head x))

-- maps adding disjunction and equivalence to whole LogicP
addDE :: LogicP -> [Form]
addDE [] = []
addDE xs = map addDE1 (sameH2 xs)

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

-- invariants in possible interpretation ([True], [False])
inv :: [[Form]] -> ([Form], [Form])
inv xs = ((breakForms (xs !! 1)), (breakForms (xs !! 2)))



trueC :: Form -> ([Form], [Form]) -> Bool
trueC _ ([], []) = False -- unknown
trueC (C a) x    = if isElem a (snd x) then False
                   else 
                        if isSublist a (fst x) then True
                        else False -- unknown

trueD :: Form -> ([Form], [Form]) -> Bool
trueD (D []) _ = False
trueD (D a) x = case a of
                     [V b, bs] -> if elem (V b) (fst x) then True else trueD (D [bs]) x
                     [N b, bs] -> if elem b (snd x) then True else trueD (D [bs]) x
                     [C b, bs] -> if trueC (C b) x then True else trueD (D [bs]) x
                     [V b]     -> if elem (V b) (fst x) then True else False
                     [N b]     -> if elem b (snd x) then True else False
                     [C b]     -> if trueC (C b) x then True else False
                     []        -> False

trueE :: Form -> ([Form], [Form]) -> Bool
trueE (E a b) x = case b of 
                       V c -> if elem (V c) (fst x) then True else False
                       N c -> if elem c (fst x) then True else False
                       C c -> trueC (C c) x
                       D c -> trueD (D c) x

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