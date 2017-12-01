module Operator where

import Formulas


isSublist :: Eq a => [a] -> [a] -> Bool                 --checks if list is a sublist of other list
isSublist [] _ = True
isSublist (x:xs) ys
                | elem x ys = isSublist xs ys
                | otherwise = False

isElem :: Eq a => [a] -> [a] -> Bool                    --checks if list contains any element of other list
isElem [] _ = False
isElem (x:xs) ys 
                | elem x ys = True
                | otherwise = isElem xs ys

opTp :: LogicP -> [Atom] -> [Atom]                      --immediate consequence operator 
opTp [] ys     = []
opTp (x:xs) ys = case x of
                    (h, pos, neg) -> if   isSublist pos ys && isElem neg ys == False 
                                     then h : opTp xs ys 
                                     else opTp xs ys

{-LogicP examples:

[(A 1, [A 3, A 5], [N (A 2), N (A 8)]), (A 11, [A 9], [N (A 13), N (A 12)]), (A 17, [A 4, A 6], [N (A 10), N (A 12)])] [A 3, A 5, A 9, A 4, A 6, A 10]
wynik: A 1, A 11
[(A 1, [A 3, A 5], [N (A 2), N (A 8)]), (A 11, [A 9], [N (A 13), N (A 12)]), (A 17, [A 4, A 6], [N (A 10), N (A 12)])] [A 3, A 5, A 9, A 4, A 6]
wynik: A 1, A 11, A 17
[(A 1, [A 3, A 5], [N (A 2), N (A 8)]), (A 11, [A 9], [N (A 13), N (A 12)]), (A 17, [A 4, A 6], [N (A 10), N (A 12)])] [A 3, A 5] 
wynik: A 1
-}