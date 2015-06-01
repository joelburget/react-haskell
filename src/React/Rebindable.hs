{-# LANGUAGE DataKinds, FlexibleInstances, OverloadedStrings #-}
module React.Rebindable where

import React.Types

-- TODO give these a proper home
(>>) :: React ty1 state sig
     -> React ty2 state sig
     -> React RtSequence state sig
(>>) = reactSeq

return :: React ty state sig -> React ty state sig
return = id

ifThenElse b x y | b = x
                 | otherwise = y


class When a where
    when :: Bool -> a -> a

instance When (React RtSequence b c) where
    when False _  = ReactTSequence []
    when True seq = seq

instance When (React RtBuiltin b c) where
    -- TODO really don't know how I feel about this instance
    -- or this typeclass in general
    when False _      = ""
    when True builtin = builtin

unless :: When a => Bool -> a -> a
unless bool = when (not bool)
