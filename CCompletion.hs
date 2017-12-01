module CCompletion where

import FormulasL

negP :: LogicP -> [Atom]
negP []     = []
negP (x:xs) = hClBodyN x ++ negP xs

--negP' :: 