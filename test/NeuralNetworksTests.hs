module NeuralNetworksTests (neuralNetworksTests) where

import ExamplesToTest
import Auxiliary
import LogicPrograms
import NeuralNetworks
import Test.Hspec
import Test.QuickCheck

neuralNetworksTests :: IO ()
neuralNetworksTests = hspec $ do
    describe "NeuralNetworks module; neuronsToPythonString" $ do
        it "for []" $
            neuronsToPythonString [] `shouldBe` "[]"
        it "for list with 2 neurons" $
            neuronsToPythonString exmplNeurons `shouldBe` neuronsString
    describe "NeuralNetworks module; nnToPythonString" $ do
        it "for empty nn" $
            nnToPythonString emptyNN `shouldBe` emptyNNstring
        it "for list with 2 neurons" $
            nnToPythonString exmplNN `shouldBe` nnString


exmplNeurons :: [Neuron]
exmplNeurons = [Neuron "A1" "tanh" 1.0 "inp1", Neuron "A2" "id" 1.5 "out1"]


neuronsString :: String
neuronsString = "[(\"A1\", \"tanh\", 1.0, \"inp1\"), (\"A2\", \"id\", 1.5, \"out1\")]"


emptyNNstring :: String
emptyNNstring = 
    "{" ++
    "\"inpLayer\" = [], " ++
    "\"hidLayer\" = [], " ++
    "\"outLayer\" = [], " ++
    "\"recLayer\" = [], " ++
    "\"inpToHidConnections\" = [], " ++
    "\"hidToOutConnections\" = [], " ++
    "\"recConnections\" = []" ++
    "}"


exmplNN :: NeuralNetwork
exmplNN = NN
    { inpLayer            = [Neuron "A1" "id" 0.0 "inp1", Neuron "A2" "id" 0.0 "inp2"]
    , hidLayer            = [Neuron "h1" "tanh" 1.0 "hid1", Neuron "h2" "tanh" 2.0 "hid2"]
    , outLayer            = [Neuron "A1" "tanh" 1.0 "out1", Neuron "A2" "tanh" 2.0 "out2"]
    , recLayer            = [Neuron "rec1" "special" 1.0 "rec1"]
    , inpToHidConnections = [Connection "inp1" "hid2" 1.0, Connection "inp2" "hid1" 2.0]
    , hidToOutConnections = [Connection "hid2" "out2" 1.0, Connection "hid1" "out1" 2.0]
    , recConnections      = [Connection "out2" "rec1" 1.0, Connection "out1" "rec1" 1.0, Connection "rec1" "inp1" 1.0]
    }


nnString :: String
nnString =
    "{" ++
    "\"inpLayer\" = [(\"A1\", \"id\", 0.0, \"inp1\"), (\"A2\", \"id\", 0.0, \"inp2\")], " ++
    "\"hidLayer\" = [(\"h1\", \"tanh\", 1.0, \"hid1\"), (\"h2\", \"tanh\", 2.0, \"hid2\")], " ++
    "\"outLayer\" = [(\"A1\", \"tanh\", 1.0, \"out1\"), (\"A2\", \"tanh\", 2.0, \"out2\")], " ++
    "\"recLayer\" = [(\"rec1\", \"special\", 1.0, \"rec1\")], " ++
    "\"inpToHidConnections\" = [(\"inp1\", \"hid2\", 1.0), (\"inp2\", \"hid1\", 2.0)], " ++
    "\"hidToOutConnections\" = [(\"hid2\", \"out2\", 1.0), (\"hid1\", \"out1\", 2.0)], " ++
    "\"recConnections\" = [(\"out2\", \"rec1\", 1.0), (\"out1\", \"rec1\", 1.0), (\"rec1\", \"inp1\", 1.0)]" ++
    "}"
