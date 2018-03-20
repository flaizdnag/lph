module LvlMap where

import Formulas
import Graph
import Operator
import Data.Graph
import Data.List
import Completion
import Examples


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