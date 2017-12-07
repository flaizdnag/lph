module CCompletion where

import FormulasL
import GraphL
import Operator
import CPL
import Data.Graph

negP :: LogicP -> [Atom]
negP []     = []
negP (x:xs) = hClBodyN x ++ negP xs

negP' :: LogicP -> [Atom]
negP' xs = intToAtom [x | x <- atomToInt (bP xs), 
                          y <- atomToInt (negP xs), 
                          path g y x && not (path g x y)]
                          where g = $ graphG xs

-- creates a program which heads belong to negP'
logicP' :: LogicP -> LogicP
logicP' xs = [x | x <- xs, isElem (hClHead x) (negP' xs)]

comp :: LogicP -> [Form]
comp []     = []
comp (x:xs) = hClToEq x : comp xs