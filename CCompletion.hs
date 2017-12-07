-- wszystko, co potrzebne do stworzenia uzupełnienia Clarka

module CCompletion where

import FormulasL
import GraphL()

negP :: LogicP -> [Atom]
negP []     = []
negP (x:xs) = hClBodyN x ++ negP xs

{-negP' :: 
negP' = [x | x <- bP, y <- negP, path (atomToInt x) (atomToInt y)]

uzupełniona definicja atomu
spójniki: negacja ~, równoważność <->, koniukcja ^, alternatywa v

osobne pliki: 
examples - przykłady programów logicznych
CPL - cpl + prover
AcceptableTest - tu będzie dużo rzeczy

nie przekraczać 80 linijek

-}