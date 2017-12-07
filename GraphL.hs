module GraphL where
    
import FormulasL
import Operator()
import Data.Graph
    
-- takes Herbrand Base (bPBase) and turns all its atoms into integers
atomToInt :: [Atom] -> [Int]
atomToInt []     = []
atomToInt (x:xs) = case x of
                        A b -> b : atomToInt xs

{-
takes Herbrand Base and returns bounds for the graph 
(requirement: Atoms in program have to be numbered in order)
-}
bounds' :: LogicP -> (Int, Int)
bounds' xs = (minimum (atomToInt (bP xs)), maximum (atomToInt (bP xs)))
    
    
zipEdges :: [Int] -> [Int] -> [(Int, Int)]        
zipEdges _ []          = []
zipEdges (x:xs) (y:ys) = (x, y) : zipEdges (x:xs) ys
    
-- creates list of pairs, each containing HC Head and one element of HC Body
edges' :: LogicP -> [(Int, Int)]
edges' [] = []
edges' (x:xs) = zipEdges (atomToInt (hClHead x)) (atomToInt (hClBody x)) ++ edges' xs
    
-- creates a graph
graphG :: LogicP -> Graph
graphG x = buildG (bounds' x) (edges' x)