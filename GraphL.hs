module Graph where

import Formulas

type Graph = ([(Atom, Atom)], [Atom, Atom])               --([positive edges][negative edges])



-- pathG :: Atom -> Atom -> Graph -> ([(Atom, Atom)], [(Atom, Atom)])
-- graphCreator :: LogicP -> Graph
-- dependsP and dependsN - Bool