import AcceptableTests
import AuxiliaryTests
import LogicProgramsTests
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
    putStrLn "Tests for 'LogicPrograms' module"
    lpTests
