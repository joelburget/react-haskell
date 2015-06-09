{-# LANGUAGE DataKinds, FlexibleInstances, OverloadedStrings #-}
module React.Rebindable where

import Data.Monoid

import React.Types
import React.Local


-- (>>) :: (GeneralizeSignal sig1 sigMax, GeneralizeSignal sig2 sigMax)
--      => React ty1 sig1
--      -> React ty2 sig2
--      -> React RtSequence sigMax
-- f1 >> f2 = ReactSequence $
--     (generalizeChildren (runReact f1)) <>
--     (generalizeChildren (runReact f2))
(>>) :: React ty1 sig
     -> React ty2 sig
     -> React RtSequence sig
f1 >> f2 = ReactSequence $ runReact f1 <> runReact f2


return :: React ty sig -> React ty sig
return = id

ifThenElse b x y | b = x
                 | otherwise = y

class When a where
    when :: Bool -> a -> a

instance When (React RtSequence sig) where
    when False _  = ReactSequence []
    when True seq = seq

instance When (React RtBuiltin sig) where
    -- TODO really don't know how I feel about this instance
    -- or this typeclass in general
    when False _      = ""
    when True builtin = builtin

unless :: When a => Bool -> a -> a
unless bool = when (not bool)
