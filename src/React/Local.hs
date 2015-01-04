{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses #-}
module React.Local (locally, Narrowing(..)) where

import Control.Applicative
import Data.Void

import React.Types

-- TODO
-- * make work with react classes

data Narrowing general local = Narrowing
    { localizeAnimationState :: AnimationState general -> AnimationState local
    , generalizeSignal :: Signal local -> Signal general
    }

locally :: Monad m
        => Narrowing general local
        -> ReactT local m x
        -> ReactT general m x
locally narrowing nested = ReactT $ \anim -> do
    (nodes, x) <- runReactT nested (localizeAnimationState narrowing anim)
    return (map (nodeConvert (generalizeSignal narrowing)) nodes, x)

handlerConvert :: (localSig -> generalSig)
               -> EventHandler localSig
               -> EventHandler generalSig
handlerConvert generalize (EventHandler handle ty) =
    EventHandler (\raw -> generalize <$> handle raw) ty

nodeConvert :: (localSig -> generalSig)
             -> ReactNode localSig
             -> ReactNode generalSig
nodeConvert generalize (Parent name attrs handlers children) =
    Parent name attrs (map (handlerConvert generalize) handlers)
        (map (nodeConvert generalize) children)
nodeConvert generalize (Leaf name attrs handlers) =
    Leaf name attrs (map (handlerConvert generalize) handlers)
nodeConvert _ (Text str) = Text str
