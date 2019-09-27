import AcceptableTests
import AuxiliaryTests
import CompletionTests
import CPLTests
import GraphTests
import LogicProgramsTests
import LPsimplifierTests
import LvlMapTests
import NeuralNetworksTests
import Test.Hspec
import Test.QuickCheck

    
main :: IO ()
main = do
    putStrLn "Tests for 'Accepatble' module"
    acceptableTests
    putStrLn ""
    putStrLn ""
    putStrLn "Tests for 'Auxiliary' module"
    auxiliaryTests
    putStrLn ""
    putStrLn ""
    putStrLn "Tests for 'Completion' module"
    completionTests
    putStrLn ""
    putStrLn ""
    putStrLn "Tests for 'CPL' module"
    cplTests
    putStrLn ""
    putStrLn ""
    putStrLn "Tests for 'Graph' module"
    graphTests
    putStrLn ""
    putStrLn ""
    putStrLn "Tests for 'LogicPrograms' module"
    lpTests
    putStrLn ""
    putStrLn ""
    putStrLn "Tests for 'LPsimplifierTests' module"
    lpSimplifierTests
    putStrLn ""
    putStrLn ""
    putStrLn "Tests for 'LvlMap' module"
    lvlMapTests
    putStrLn ""
    putStrLn ""
    putStrLn "Tests for 'NeuralNetworks' module"
    neuralNetworksTests
