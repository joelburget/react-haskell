{-# LANGUAGE DataKinds, FlexibleInstances, OverloadedStrings,
    MultiParamTypeClasses #-}
module React.Rebindable where

import Data.Monoid

import React.Types


(>>) :: ReactNode sig -> ReactNode sig -> ReactNode sig
(>>) = (<>)

return :: ReactNode sig -> ReactNode sig
return = id

ifThenElse b x y | b = x
                 | otherwise = y

when :: Bool -> ReactNode sig -> ReactNode sig
when False _  = undefined
when True seq = seq

unless :: Bool -> ReactNode sig -> ReactNode sig
unless bool = when (not bool)
