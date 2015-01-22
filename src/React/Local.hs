{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses,
  FlexibleContexts, IncoherentInstances #-}
-- Note on IncoherentInstances: the two instances below will both work fine
-- for `GeneralizeSignal Void Void`. They should never be called.
module React.Local (locally, GeneralizeSignal(..)) where

import Control.Applicative
import Data.Void

import React.Types

-- TODO
-- * make work with react classes

class GeneralizeSignal sigloc siggen where
    generalizeSignal :: sigloc -> siggen


instance GeneralizeSignal a a where
    generalizeSignal = id


instance GeneralizeSignal Void a where
    generalizeSignal = absurd


locally :: (Monad m, GeneralizeSignal sigloc siggen)
        => ReactT stateloc sigloc anim m x
        -> ReactT stategen siggen anim m x
locally nested = result where
    result = ReactT $ \anim -> do
        let gensig = nodeConvert generalizeSignal
        (nodes, x) <- runReactT nested anim
        return (map gensig nodes, x)


handlerConvert :: (sigloc -> siggen)
               -> EventHandler sigloc
               -> EventHandler siggen
handlerConvert generalize (EventHandler handle ty) =
    EventHandler (\raw -> generalize <$> handle raw) ty


nodeConvert :: (sigloc -> siggen)
             -> ReactNode sigloc
             -> ReactNode siggen
nodeConvert generalize (Parent f attrs handlers children) =
    Parent f attrs (map (handlerConvert generalize) handlers)
        (map (nodeConvert generalize) children)
nodeConvert generalize (Leaf f attrs handlers) =
    Leaf f attrs (map (handlerConvert generalize) handlers)
nodeConvert _ (Text str) = Text str
