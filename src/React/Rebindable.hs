{-# LANGUAGE DataKinds, FlexibleInstances, OverloadedStrings #-}
module React.Rebindable where

import Data.Monoid

import React.Types
import React.Local


-- (>>) :: (GeneralizeSignal sig1 sigMax, GeneralizeSignal sig2 sigMax)
--      => ReactElement ty1 sig1
--      -> ReactElement ty2 sig2
--      -> ReactElement RtSequence sigMax
-- f1 >> f2 = ReactSequence $
--     (generalizeChildren (runReact f1)) <>
--     (generalizeChildren (runReact f2))
(>>) :: ReactElement ty1 sig
     -> ReactElement ty2 sig
     -> ReactElement RtSequence sig
f1 >> f2 = ReactSequence $ runReact f1 <> runReact f2


return :: ReactElement ty sig -> ReactElement ty sig
return = id

ifThenElse b x y | b = x
                 | otherwise = y

class When a where
    when :: Bool -> a -> a

instance When (ReactElement RtSequence sig) where
    when False _  = ReactSequence []
    when True seq = seq

instance When (ReactElement RtBuiltin sig) where
    -- TODO really don't know how I feel about this instance
    -- or this typeclass in general
    when False _      = ""
    when True builtin = builtin

unless :: When a => Bool -> a -> a
unless bool = when (not bool)
