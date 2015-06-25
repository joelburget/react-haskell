{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses,
  FlexibleContexts, IncoherentInstances, LambdaCase, DataKinds #-}
-- Note on IncoherentInstances: the two instances below will both work fine
-- for `GeneralizeSignal Void Void`. They should never be called.
module React.Local (GeneralizeSignal(..), locally) where

import Control.Applicative
import Data.Void

import React.Types


-- |
-- Implement when a local signal can be generalized to a higher level one. Used
-- by 'locally'
class GeneralizeSignal sigloc siggen where
    generalizeSignal :: sigloc -> siggen


instance GeneralizeSignal a a where
    generalizeSignal = id


instance GeneralizeSignal Void a where
    generalizeSignal = absurd


-- |
-- 'locally' exists to make it simple to embed classes with local concerns. An
-- example will be helpful:
--
-- We have some page which can respond to many different events.
--
-- @
-- data Transition
--     = UserTyping JSString
--     | Toggle
--     ...
--
-- globalPage_ :: [AttrOrHandler Transition] -> ReactNode Transition
-- @
--
-- And we want to be able to embed some components that don't care about all
-- that. 'inputBox_' can only output 'JSString' and 'pageHeader_' can't send
-- any signals at all.
--
-- @
-- inputBox_ :: ReactNode JSString
-- pageHeader_ :: ReactNode Void
-- @
--
-- With locally we can easily embed them in 'globalPage_':
--
-- @
-- instance GeneralizeSignal JSString Transition where
--     generalizeSignal = UserTyping
--
-- -- (globalPage_)
-- renderFn = \props state -> div_ [ class_ "global-page" ] $ do
--     locally pageHeader_
--     ...
--     locally inputBox_
--     ...
-- @
locally :: GeneralizeSignal sigloc siggen
        => ReactNode sigloc
        -> ReactNode siggen
locally = LocalNode generalizeSignal
