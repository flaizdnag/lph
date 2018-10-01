{-|
Module      : FindOut
Description : Tools needed to read a logic program from a '.txt' file.
Copyright   : (c) Andrzej G., 2017
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Tools needed to read a logic program from a '.txt' file.
-}
module FindOut
    () where

import System.IO  
import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP


-- | Reader of the file content.
reader :: IO ()
reader = do
    f <- readFile "exemplary.txt"
    let legend = takeWhile (/= "") (lines f)
    mapM_ print $ legend

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

letter :: ReadP Char
letter = satisfy (\char -> (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z'))

number :: ReadP Int
number = do
    num <- fmap read (many1 digit)
    return num

text :: ReadP String
text = do
    txt <- many1 letter
    return txt

legendItem :: ReadP (Int, String)
legendItem = do
    num <- number
    string " - "
    label <- text
    eof
    return (num, label)
--(map fst $ concatMap (readP_to_S legendItem) (takeWhile (/= "") (lines f)))
