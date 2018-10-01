{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : ThreeValuedSem
Description : Module defining three-valued semantics
Copyright   : (c) Andrzej G., 2017-
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Longer description
-}
module ThreeValuedSem
    ( ThreeValues (..)
    , ThreeValuedSemantic (..)
    ) where


class ThreeValuedSemantic a b where
    eval3v :: a -> b -> ThreeValues


data ThreeValues = Tr3v | Fa3v | Un3v
    deriving (Show, Read, Eq)
