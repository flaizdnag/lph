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

Level mapping of an atom (or literal) is a number associated with this atom (or
literal). This module contains functions for generation of all possible level
mappings for a given logic program, and checking the value of a given atom in
a given level mapping.
-}
module LvlMap
    ( possibleLvLMaps
    , lvlMapValue
    ) where

import LogicPrograms
import Data.List (permutations, (\\), lookup)


-- | Generates all possible level mappings for a given logic program, where the
-- assumption is that permutations concern only those atoms that occur as heads
-- and in the bodies of clauses.
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
lvlMapValue :: Atom -> [(Atom, Int)] -> Int
lvlMapValue a lvlM = case lookup a lvlM of
    Nothing -> -404
    Just n  -> n
