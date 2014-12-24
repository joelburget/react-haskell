{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses,
  ScopedTypeVariables #-}
module React.Local (locally) where

import Control.Applicative

import React.Types

class ReactLocal a b where
    locally :: a -> b

instance (Monad m, local ~ local', general ~ general') =>
    ReactLocal (local -> general)
               (ReactT anim local' m x -> ReactT anim general' m x) where

    locally = locallyFocus

instance (Monad m, m ~ m', x ~ x') =>
    ReactLocal (ReactT anim () m x) (ReactT anim general m' x') where

    locally = locallyEmpty

handlerConvert :: (local -> general)
               -> EventHandler local
               -> EventHandler general
handlerConvert f (EventHandler handle ty) =
    EventHandler (\raw -> f <$> handle raw) ty

handlerConvert' :: EventHandler () -> EventHandler general
handlerConvert' (EventHandler handle ty) = EventHandler (const Nothing) ty

nodeConvert1 :: (local -> general) -> ReactNode local -> ReactNode general
nodeConvert1 f (Parent name attrs handlers children) =
    Parent name attrs (map (handlerConvert f) handlers)
        (map (nodeConvert1 f) children)
nodeConvert1 f (Leaf name attrs handlers) =
    Leaf name attrs (map (handlerConvert f) handlers)
nodeConvert1 f (Text str) = Text str

nodeConvert2 :: ReactNode () -> ReactNode general
nodeConvert2 (Parent name attrs handlers children) =
    Parent name attrs (map handlerConvert' handlers)
        (map nodeConvert2 children)
nodeConvert2 (Leaf name attrs handlers) =
    Leaf name attrs (map handlerConvert' handlers)
nodeConvert2 (Text str) = Text str

locallyFocus :: Monad m
             => (local -> general)
             -> ReactT anim local m x
             -> ReactT anim general m x
locallyFocus localize nested = ReactT $ \anim -> do
    (nodes, x) <- runReactT nested [] -- XXX [] vs anim
    return (map (nodeConvert1 localize) nodes, x)

locallyEmpty :: forall anim general m x. Monad m
             => ReactT anim () m x
             -> ReactT anim general m x
locallyEmpty nested = ReactT $ \anim -> do
    (nodes, x) <- runReactT nested [] -- XXX [] vs anim
    return (map nodeConvert2 nodes, x)
