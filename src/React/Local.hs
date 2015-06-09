{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses,
  FlexibleContexts, IncoherentInstances, LambdaCase, DataKinds #-}
-- Note on IncoherentInstances: the two instances below will both work fine
-- for `GeneralizeSignal Void Void`. They should never be called.
module React.Local (GeneralizeSignal(..), generalizeChildren, locally) where

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


-- class SignalMax sig1 sig2 siggen where


-- instance (GeneralizeSignal sigloc siggen, GeneralizeSignal siggen siggen) =>
--     SignalMax sigloc siggen siggen where
-- instance (GeneralizeSignal sigloc siggen, GeneralizeSignal siggen siggen) =>
--     SignalMax siggen sigloc siggen where


locally :: GeneralizeSignal sigloc siggen
        => ReactElement ty sigloc
        -> ReactElement RtSequence siggen
locally = ReactSequence . generalizeChildren . runReact


generalizeChildren :: GeneralizeSignal sigloc siggen
                   => [Child sigloc]
                   -> [Child siggen]
generalizeChildren = map (childConvert generalizeSignal)


childConvert :: (sigloc -> siggen) -> Child sigloc -> Child siggen
childConvert generalize =
    let gensig = nodeConvert generalize
    in \case
        Static node -> Static (gensig node)
        Dynamic nodes -> Dynamic (map (\(i, node) -> (i, gensig node)) nodes)


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
        (map (childConvert generalize) children)
nodeConvert generalize (Leaf f attrs handlers) =
    Leaf f attrs (map (handlerConvert generalize) handlers)
nodeConvert _ (Text str) = Text str
