module CCompletion where

import Formulas

negP :: LogicP -> [Atom]
negP []     = []
negP (x:xs) = hClBodyN x ++ negP xs

--negP' :: 