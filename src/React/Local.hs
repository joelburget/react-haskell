{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses,
  FlexibleContexts, IncoherentInstances, LambdaCase, DataKinds #-}
-- Note on IncoherentInstances: the two instances below will both work fine
-- for `GeneralizeSignal Void Void`. They should never be called.
module React.Local (GeneralizeSignal(..), locally) where

import Control.Applicative
import Data.Void

import React.Types


class GeneralizeSignal sigloc siggen where
    generalizeSignal :: sigloc -> siggen


instance GeneralizeSignal a a where
    generalizeSignal = id


instance GeneralizeSignal Void a where
    generalizeSignal = absurd


locally :: GeneralizeSignal sigloc siggen
        => ReactNode sigloc
        -> ReactNode siggen
locally = LocalNode generalizeSignal
