{-|
Module      : LvlMap
Description : Tools needed to create a level mapping function for a logic
              program.
Copyright   : (c) Aleksandra Cz., 2017
                  Kinga O., 2017
                  Andrzej G., 2017
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Longer description
-}
module LvlMap
    ( possibleLvLMaps
    , lvlMVal
    ) where

import LogicPrograms
import Data.List (permutations, (\\), lookup)
--import qualified Data.Map.Strict as Map


-- | Generates all possible level mappings for a given logic program, where the
-- assumption is that permutations concern only those atoms that occur as heads
-- and in the bodies of Horn clauses.
possibleLvLMaps :: LP -> [[(Atom, Int)]]
possibleLvLMaps lp = [ xs |
    let n   = length (bp lp),
    let bn  = length (onlyBodies lp),
    let hn  = length (onlyHeads lp),
    let bs  = zip (onlyBodies lp) [1..],
    let hs  = zip (onlyHeads lp) [n, n-1..],
    rs <- [ zs |
        let remA    = bp lp \\ (onlyBodies lp ++ onlyHeads lp),
        remIperm    <- permutations [bn+1..n-hn],
        let zs      = zip remA remIperm ],
    let xs  = bs ++ hs ++ rs ]

-- | Takes an atom and a level mapping and returns the value assigned to the
-- atom.
lvlMVal :: Atom -> [(Atom, Int)] -> Int
lvlMVal a lvlM = case lookup a lvlM of
    Nothing -> 0
    Just n  -> n

--------------------------------------------------------------------------------
-- Old code --- soon will disappear                                           --
--------------------------------------------------------------------------------

{-
-- | generates list of subsequent numbers as long as given list
numList :: [Atom] -> [Int]
numList [] = []
numList xs = [x | x <- [1..n]]
             where n = length xs

-- | checks if head does not appear in bodies
onlyHead :: LogicP -> Atom -> Bool
onlyHead [] _ = False
onlyHead xs y = if elem y (bPBodies xs) then False
                else True

-- | checks if atom appears only in bodies
onlyBody :: LogicP -> Atom -> Bool
onlyBody [] _ = False
onlyBody xs y = if elem y (bPHeads xs) then False
                else True

-- | sorts Bp elements into 3 lists in order: 
-- elements that appear only in heads,
-- elements that appear both in heads and bodies
-- elements that appear only in bodies.
sortElems :: LogicP -> ([Atom], [Atom], [Atom])
sortElems xs = ([x | x <- (bPHeads xs), onlyHead xs x], 
                [y | y <- (bPHeads xs), (onlyHead xs y) == False], 
                [z | z <- (bPBodies xs), onlyBody xs z])


-- | replaces atoms with assigned numbers
mapNum :: ([Atom], [Atom], [Atom]) -> [Int] -> ([Int], [Int], [Int])
mapNum (a, b, c) xs = (x, y, z)
                            where
                                z = (take (length c) xs)
                                x = (drop ((length xs) - (length a)) xs)
                                y = (xs \\ (x ++ z))

mapNum' :: LogicP -> ([Int], [Int], [Int])
mapNum' xs = mapNum (sortElems xs) (numList (bP xs))

-- | returns permutations of list
permN :: ([Atom], [Atom], [Atom]) -> [[Atom]]
permN (a, b, c) = permutations b

-- checker??

-- | replaces middle list with one of the permutations (depended on checker)
replaceNum :: ([Atom], [Atom], [Atom]) -> [[Atom]] -> ([Atom], [Atom], [Atom])
replaceNum (a, b, c) (x:xs) = (a, x, c)

{-
lvlMap (x:xs) = if checker (mapNum (a, x, c)) 
                then mapNum (a, x, c) 
                else lvlMap xs
                -}
-}
