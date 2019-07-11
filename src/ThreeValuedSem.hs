{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : ThreeValuedSem
Description : Module defining three-valued semantics
Copyright   : (c) Andrzej G., 2017-
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Module containing classes for three-valued semantics.
-}
module ThreeValuedSem
    ( ThreeValues (..)
    , StrongKleeneSemantics (..)
    , LukasiewiczSemantic (..)
    ) where


class StrongKleeneSemantics a b where
    evalStrKleene :: a -> b -> ThreeValues


class LukasiewiczSemantic a b where
    evalLukasiewicz :: a -> b -> ThreeValues


data ThreeValues = Tr3v | Fa3v | Un3v
    deriving (Show, Read, Eq)
