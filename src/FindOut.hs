{-|
Module      : FindOut
Description : Tools needed to read a logic program from a '.txt' file.
Copyright   : (c) Andrzej G., 2017
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Longer description
-}
module FindOut
    () where

import System.IO  
import Control.Monad
import Control.Applicative ((<|>))
import Data.Char
import Data.Graph
import Graph
import Data.Array   (assocs)
import Text.ParserCombinators.ReadP


data ReportFO = ReportFO
    { code :: String
    , column1 :: [(Integer, String)]
    , column2 :: [(Integer, String)]
    , situations :: [(Integer, String)]
    , graph :: Graph
    }
    deriving Show


-- | Reader of the file content.
reader :: IO ()
reader = do
    content <- readFile "exemplary.txt"
    let result = reportToLaTeX (fst $ head (readP_to_S parserFO content))
    putStr result

-- | Parser from string format to ReportFO.
parserFO :: ReadP ReportFO
parserFO = do
    sbjCode <- getSCode
    col1 <- getCols
    col2 <- getCols
    situations <- getSituations
    graphEdges <- fmap read textEOL
    return (ReportFO sbjCode (zip [1..] col1) (zip [1..] col2) (zip [1..] situations) (buildG (0, length situations) graphEdges))

getSCode :: ReadP String
getSCode = do
    string "["
    code <- many1 (satisfy (\char -> isUpper char || isAlphaNum char))
    string "]"
    count 2 eol
    return code

getCols :: ReadP [String]
getCols = do
    string "KOLUMNA 1" <|> string "KOLUMNA 2"
    eol
    item <- many1 getItem
    count 2 eol
    return item

getSituations :: ReadP [String]
getSituations = do
    situation <- many1 getItem
    eol
    return situation

getItem :: ReadP String
getItem = do
    many1 digit
    string ". " <|> string " - "
    item <- many1 (satisfy (\char -> char /= '\n'))
    eol
    return item

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

textEOL :: ReadP String
textEOL = do
    txt <- many1 (satisfy (\char -> char /= '\n'))
    eol
    return txt

eol :: ReadP Char
eol = satisfy (\char -> char == '\n')

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

exTEXT :: String
exTEXT = "[KAJ08]\n\nKOLUMNA 1\n1. CELEBRYTA FAKTYCZNIE WYPIł WINO, KT\211REGO OTWART\260 BUTELK\280 WI\211Z\321 W SAMOCHODZIE. NIE OK\321AMA\321 WI\280C POLICJANT\211W. JEDNAK\379E NIE BY\321 \346WIADOMY, \379E WINO NIE ZAWIERA\321O ALKOHOLU. TO T\321UMACZY DLACZEGO MIMO PRZYZNANIA SI\280 DO WYKROCZENIA PRAWA, NIE ZOSTA\321 ZATRZYMANY I M\211G\321 POJECHA\262 DALEJ. NAST\280PNIE POLICJANCI NARADZILI SI\280, ABY POJECHA\262 DO MIASTA I ZBADA\262 SPRAW\280 OSZUKANEGO ALKOHOLU OD LOKALNEGO WYTW\211RCY REGIONALNYCH SPECJA\321\211W.\n2. DRUGI ITEM \n3. TRZECI ITEM\n\n\nKOLUMNA 2\n1. WINO NIE ZAWIERA\321O ALKOHOLU.\n2. ALKOMAT NIC NIE WYKAZA\321, A BY\321 SPRAWNY.\n3. CELEBRYTA ODJECHA\321 BEZ PROBLEM\211W. \n4. POLICJANCI NARADZALI SI\280 PO TYM JAK CELEBRYTA JU\379 ODJECHA\321, A NAST\280PNIE UDALI SI\280 DO CENTRUM MIASTA.\n\n\n1 - CELEBRYTA WYPI\321 WINO [BI]\n2 - CELEBRYTA WI\211Z\321 W SAMOCHODZIE OTWART\260 BUTELK\280 WINA [AH]\n3 - CELEBRYTA NIE BY\321 \346WIADOMY, \379E WINO NIE ZAWIERA\321O ALKOHOLU [AI]\n4 - WINO NIE ZAWIERA\321O ALKOHOLU [AI]\n5 - CELEBRYTA PRZYZNA\321 SI\280 DO WYPICIA WINA [BH]\n6 - CELEBRYTA NIE ZOSTA\321 ZATRZYMANY I M\211G\321 POJECHA\262 DALEJ; ODJECHA\321 BEZ PROBLEM\211W [BH]\n7 - POLICJANCI NARADZILI SI\280, ABY POJECHA\262 DO MIASTA ZBADA\262 SPRAW\280 OSZUKANEGO ALKOHOLU OD LOKALNEGO WYTW\211RCY REGIONALNYCH SPECJA\321\211W [BHZ]\n8 - ALKOMAT NIC NIE WYKAZA\321 [BI] \n9 - ALKOMAT BY\321 SPRAWNY [AI]\n10 - POLICJANCI UDALI SI\280 DO CENTRUM [BH]  \n\n[(0,1), (0,2), (0,3), (0,4), (0,9), (1,5), (5,8), (8,6), (6,7), (7,10)]\n"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

latexWriterTest :: IO()
latexWriterTest = putStr $ reportToLaTeX $ fst $ head (readP_to_S parserFO exTEXT)

reportToLaTeX :: ReportFO -> String
reportToLaTeX (ReportFO code col1 col2 sts graph) =
    "Nr osoby badanej: " ++ code ++ "\n\n" ++
    "Kolumna 1:\n" ++
    "\\begin{enumerate}\n" ++
    itemized col1 ++
    "\\end{enumerate}\n\n" ++
    "Kolumna 2:\n" ++
    "\\begin{enumerate}\n" ++
    itemized col2 ++
    "\\end{enumerate}\n\n" ++
    "Sytuacje:\n" ++
    "\\begin{enumerate}\n" ++
    itemized sts ++
    "\\end{enumerate}\n\n" ++
    show graph

itemized :: [(Integer, String)] -> String
itemized []     = ""
itemized (x:xs) = "    \\item " ++ snd x ++ "\n" ++ itemized xs

fbf :: String -> String
fbf s = "\\textbf{" ++ s ++ "}"

{-
graphToForest :: Graph -> String
graphToForest g =
    "[\n" ++
    "0" ++ succ
    "]"
-}

graphToForest :: Int -> [(Int, [Int])] -> String
graphToForest n x:xs =
    | succG n x:xs == [] = "[" ++ show n ++ "]"
    | otherwise          = "[" ++ show n ++ graphToForest  ++ "]"

succG :: Int -> [(Int, [Int])] -> [Int]
succG n xs = case lookup n xs of
    Nothing -> []
    Just ys -> ys

exGraph :: Graph
exGraph = buildG (0,10) [(0,1), (0,2), (0,3), (0,4), (0,9), (1,5), (5,8), (8,6), (6,7), (7,10)]
