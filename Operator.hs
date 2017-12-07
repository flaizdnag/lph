module Operator where

import FormulasL
import Data.List


--checks if list contains any element of other list
isElem :: Eq a => [a] -> [a] -> Bool
isElem [] _ = False
isElem (x:xs) ys 
                | elem x ys = True
                | otherwise = isElem xs ys

--immediate consequence operator
opTp :: LogicP -> [Atom] -> [Atom] 
opTp [] _      = []
opTp (x:xs) ys = case x of
                    (h, pos, neg) -> if   isInfixOf pos ys && isElem neg ys == False 
                                     then h : opTp xs ys 
                                     else opTp xs ys


{-LogicP examples:

[(A 1, [A 3, A 5], [A 2, A 8]), (A 11, [A 9], [A 13, A 12]), (A 17, [A 4, A 6], [A 10, A 12])] [A 3, A 5, A 9, A 4, A 6, A 10]
wynik: A 1, A 11
[(A 1, [A 3, A 5], [A 2, A 8]), (A 11, [A 9], [A 13, A 12]), (A 17, [A 4, A 6], [A 10, A 12])] [A 3, A 5, A 9, A 4, A 6]
wynik: A 1, A 11, A 17
[(A 1, [A 3, A 5], [A 2, A 8]), (A 11, [A 9], [A 13, A 12]), (A 17, [A 4, A 6], [A 10, A 12])] [A 3, A 5] 
wynik: A 1
-}