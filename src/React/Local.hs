{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses #-}
module React.Local (locally) where

import Control.Applicative

import React.Types

class GeneralizePage local general where
    localizeClassState :: ClassState general -> ClassState local
    localizeAnimationState :: AnimationState general -> AnimationState local

    generalizeSignal :: Signal local -> Signal general

instance GeneralizePage () general where
    localizeClassState _ = UnitClassState
    localizeAnimationState _ = UnitAnimationState

    generalizeSignal _ = undefined

class ReactLocal local general where
    locally :: local -> general

-- instance (Monad m, GeneralizePage local general, local ~ local', general ~ general') =>
instance (Monad m, GeneralizePage local general) =>
    ReactLocal (ReactT local m x) (ReactT general m x) where

    locally = locallyFocus

-- instance (Monad m, m ~ m', x ~ x') =>
--     ReactLocal (ReactT anim () m x) (ReactT anim general m' x') where
--
--     locally = locallyEmpty

handlerConvert :: GeneralizePage local general
               => EventHandler (Signal local)
               -> EventHandler (Signal general)
handlerConvert (EventHandler handle ty) =
    EventHandler (\raw -> generalizeSignal <$> handle raw) ty

nodeConvert1 :: GeneralizePage local general
             => ReactNode (Signal local)
             -> ReactNode (Signal general)
nodeConvert1 (Parent name attrs handlers children) =
    Parent name attrs (map handlerConvert handlers)
        (map nodeConvert1 children)
nodeConvert1 (Leaf name attrs handlers) =
    Leaf name attrs (map handlerConvert handlers)
nodeConvert1 (Text str) = Text str

locallyFocus :: (Monad m, GeneralizePage local general)
             => ReactT local m x
             -> ReactT general m x
locallyFocus nested = ReactT $ \anim -> do
    (nodes, x) <- runReactT nested (localizeAnimationState anim)
    return (map nodeConvert1 nodes, x)

-- handlerConvert' :: EventHandler () -> EventHandler general
-- handlerConvert' (EventHandler handle ty) = EventHandler (const Nothing) ty

-- nodeConvert2 :: ReactNode () -> ReactNode general
-- nodeConvert2 (Parent name attrs handlers children) =
--     Parent name attrs (map handlerConvert' handlers)
--         (map nodeConvert2 children)
-- nodeConvert2 (Leaf name attrs handlers) =
--     Leaf name attrs (map handlerConvert' handlers)
-- nodeConvert2 (Text str) = Text str

-- locallyEmpty :: Monad m
--              => ReactT () m x
--              -> ReactT general m x
-- locallyEmpty nested = ReactT $ \anim -> do
--     (nodes, x) <- runReactT nested anim
--     return (map nodeConvert2 nodes, x)
