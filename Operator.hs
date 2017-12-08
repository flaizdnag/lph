module Operator where

import Formulas
import Data.List
--import GHC.Exts


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
                    (h, pos, neg) -> if   isSubsequenceOf pos ys && isElem neg ys == False 
                                     then h : opTp xs ys 
                                     else opTp xs ys

{- iterations of Immediate Consequence Operator (Tp)
where:
fixpoint - takes logic program
           applies iterations of Tp, starting with index 1
           returns effect of iterations - Herbrand model

iteration - takes logic program and effect of first Tp iteration (from fixpoint)
            iterates Tp until results are repeated
            returns effect of iterations as list of atoms (Herbrand model) and last index
            (might loop)

fxp - takes logic program and index of iteration,
      returns pair with list of atoms (Tp result) and incremented index
      -}
fxp :: LogicP -> Int -> ([Atom], Int)
fxp x 0 = (opTp x [], 0)
fxp x n = (opTp x (fst (fxp x (n - 1))), (n + 1))
      
iteration :: LogicP -> ([Atom], Int) -> ([Atom], Int)
iteration x (y, n) = if a == b
                        then fxp x n
                        else iteration x (fxp x (n + 1))
                            where
                                a = fst (fxp x n)
                                b = fst (fxp x (n - 1))

fixpoint :: LogicP -> [Atom]
fixpoint x = fst (iteration x (fxp x 1))

{-
interps :: LogicP -> [[Atom]] 
interps x = sortWith length $ subsequences (bP x)
-}

{-LogicP examples:

[(A 1, [A 3, A 5], [A 2, A 8]), (A 11, [A 9], [A 13, A 12]), (A 17, [A 4, A 6], [A 10, A 12])] [A 3, A 5, A 9, A 4, A 6, A 10]
wynik: A 1, A 11
[(A 1, [A 3, A 5], [A 2, A 8]), (A 11, [A 9], [A 13, A 12]), (A 17, [A 4, A 6], [A 10, A 12])] [A 3, A 5, A 9, A 4, A 6]
wynik: A 1, A 11, A 17
[(A 1, [A 3, A 5], [A 2, A 8]), (A 11, [A 9], [A 13, A 12]), (A 17, [A 4, A 6], [A 10, A 12])] [A 3, A 5] 
wynik: A 1
-}
                                
