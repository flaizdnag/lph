module Graph where

import Formulas
import Operator

type Graph = ([(Atom, Atom)], [(Atom, Atom)])               --([positive edges][negative edges])

zipEdges :: Atom -> [Atom] -> [(Atom, Atom)]        --zips head with body elements
zipEdges _ []     = []
zipEdges a (x:xs) = (a, x) : zipEdges a xs

graphP :: LogicP -> [(Atom, Atom)]                  --makes positive edges
graphP []     = []
graphP (x:xs) = case x of
                        (a, b, _) -> (zipEdges a b) ++ graphP xs

graphN :: LogicP -> [(Atom, Atom)]                  --makes negative edges
graphN []     = []
graphN (x:xs) = case x of
                        (a, _, c) -> (zipEdges a c) ++ graphN xs

graphCreator :: LogicP -> Graph                     --creates a graph
graphCreator []     = ([], [])
graphCreator xs = (graphP xs, graphN xs)



-- pathG :: Atom -> Atom -> Graph -> ([(Atom, Atom)], [(Atom, Atom)])
-- graphCreator :: LogicP -> Graph
-- dependsP and dependsN -> Bool
{- unzip :: [(a, b)] -> ([a], [b]) transforms a list of pairs into a list of first components and a list of second components.-}