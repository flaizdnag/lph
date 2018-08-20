{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import Formulas
import Completion
import Web.Scotty               (scotty, post, body, text, ActionM)
import Data.Text.Lazy           (Text, pack, unpack)
import Data.Text.Lazy.Encoding  (decodeUtf8)

main :: IO ()
main = scotty 10100 $ do
    
    post "/api/bP" $ do
        b <- decodeUtf8 <$> body
        responseLP b bP

    post "/api/positiveBodies" $ do
        b <- decodeUtf8 <$> body
        responseLP b bPBodiesP

    post "/api/negativeBodies" $ do
        b <- decodeUtf8 <$> body
        responseLP b bPBodiesN

    post "/api/completion" $ do
        b <- decodeUtf8 <$> body
        responseLP b comp

responseLP :: Show a => Text -> (LogicP -> a) -> Web.Scotty.ActionM ()
responseLP x f = textPackShow $ f $ readUnpackLP x
    where
        readUnpackLP = \x -> read (unpack x) :: LogicP
        textPackShow = \x -> text $ pack $ show x
