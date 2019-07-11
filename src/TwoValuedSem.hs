{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : TwoValuedSem
Description : Module defining two-valued semantics
Copyright   : (c) Andrzej G., 2017-
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Module containing class for two-valued semantics.
-}
module TwoValuedSem
    ( TwoValues (..)
    , TwoValuedSemantic (..)
    ) where


class TwoValuedSemantic a b where
    eval2v :: a -> b -> TwoValues


data TwoValues = Tr2v | Fa2v
    deriving (Show, Read, Eq)
