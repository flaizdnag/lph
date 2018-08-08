{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import Web.Scotty
import Formulas
import Control.Applicative     ((<$>))
import Data.Aeson.TH           (defaultOptions, deriveJSON)
import Data.Monoid             (mconcat, (<>))
import Data.Text.Lazy          (Text, pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)

main :: IO ()
main = scotty 10100 $ do

  post "/api/bP" $ do
      b <- decodeUtf8 <$> body
      let a = read (unpack b) :: LogicP
      text $ pack $ show (bP a)
