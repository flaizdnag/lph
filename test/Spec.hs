import           AcceptableTests
import           AuxiliaryTests
import           CPLTests
import           CompletionTests
import           GraphTests
import           LPsimplifierTests
import           LogicProgramsTests
import           LvlMapTests
import           NNdecoderTests
import           NeuralNetworksTests
import           PhiOperatorTests
import           Test.Hspec
import           Test.QuickCheck
import           TpOperatorTests
import           TranslationTpTests

    
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
    putStrLn ""
    putStrLn ""
    --putStrLn "Tests for 'NNdecoder' module"
    --nnDecoderTests
    --putStrLn ""
    --putStrLn ""
    putStrLn "Tests for 'PhiOperator' module"
    phiOperatorTests
    putStrLn ""
    putStrLn ""
    putStrLn "Tests for 'TpOperator' module"
    tpOperatorTests
    putStrLn ""
    putStrLn ""
    --putStrLn "Tests for 'TranslationTp' module"
    --translationTpTests
