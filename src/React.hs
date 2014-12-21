{-# LANGUAGE OverloadedStrings, FlexibleInstances, LambdaCase,
  MultiParamTypeClasses, FlexibleContexts, Rank2Types, GADTs,
  GeneralizedNewtypeDeriving, ForeignFunctionInterface #-}

module React
    ( module X
    , render
    , ReactLocal(locally)
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.IORef
import Data.Monoid
import Data.String

import Haste hiding (fromString)
import Haste.Foreign
import Haste.JSON
import Haste.Prim

import Prelude hiding (div)

import React.Attrs as X
import React.Elements as X
import React.Events as X
import React.Imports
import React.Types as X

-- TODO
-- * restricted monads
-- * store elem in monad
-- * escaping / dangerouslySetInnerHTML

element :: (JSString -> RawAttrs -> ReactArray -> IO ForeignNode)
        -> JSString
        -> Attrs
        -> [(RawEvent -> Maybe (IO ()), EvtType)]
        -> [ForeignNode]
        -> IO ForeignNode
element constructor name attrs handlers content = do
    attr <- js_empty_object
    mapM_ (setField attr) attrs
    mapM_ (makeHandler attr) handlers

    children <- js_ReactArray_empty
    mapM_ (js_ReactArray_push children) content
    constructor name attr children

voidElement :: (JSString -> RawAttrs -> IO ForeignNode)
            -> JSString
            -> Attrs
            -> [(RawEvent -> Maybe (IO ()), EvtType)]
            -> IO ForeignNode
voidElement constructor name attrs handlers =
    element (\n a c -> constructor n a) name attrs handlers []

setField :: RawAttrs -> (JSString, JSON) -> IO ()
setField attr (fld, Str v) = js_set_field_String attr fld v
setField attr (fld, Dict vs) = do
    subObj <- js_empty_object
    mapM_ (setField subObj) vs
    js_set_field_Obj attr fld subObj
setField attr (fld, Num v) = js_set_field_Double attr fld v
setField attr (fld, Bool True) = js_set_field_True attr fld
setField attr (fld, Bool False) = js_set_field_False attr fld

-- TODO this seems wrong
setField attr (fld, Null) = return ()

-- TODO figure out what to do with this
-- getDomNode :: ForeignNode -> IO (Maybe Elem)
-- getDomNode r = fmap fromPtr (js_React_getDomNode r)

interpret :: Monad m
          => ReactT u m ()
          -> (u -> IO ())
          -> m (IO ForeignNode)
interpret react cb = do
    ~(child:_, ()) <- runReactT react
    return $ interpret' cb child

interpret' :: (u -> IO ())
           -> ReactNode u
           -> IO ForeignNode
interpret' cb = \case
    Parent name as hs children -> do
        children' <- forM children (interpret' cb)
        let hs' = map (unHandler cb) hs
        element js_React_DOM_parent name as hs' children'
    Leaf name as hs -> do
        let hs' = map (unHandler cb) hs
        voidElement js_React_DOM_leaf name as hs'
    Text str -> js_React_DOM_text (toJSStr str)

class ReactLocal a b where
    locally :: a -> b

instance (Monad m, local ~ local', general ~ general') =>
    ReactLocal (local -> general) (ReactT local' m x -> ReactT general' m x) where

    locally = locally1

instance (Monad m, m ~ m', x ~ x') =>
    ReactLocal (ReactT () m x) (ReactT general m' x') where

    locally = locally2

locally1 :: Monad m
         => (local -> general)
         -> ReactT local m x
         -> ReactT general m x
locally1 localize nested = ReactT $ do
    (nodes, x) <- runReactT nested
    return (map (nodeConvert1 localize) nodes, x)

locally2 :: Monad m
         => ReactT () m x
         -> ReactT general m x
locally2 nested = ReactT $ do
    (nodes, x) <- runReactT nested
    return (map nodeConvert2 nodes, x)

render' :: Elem -> ForeignNode -> IO ()
render' = ffi "(function(e,r){React.render(r,e);})"

-- om's adding of raf:
-- https://github.com/swannodette/om/commit/ec5f7890d85295e8ffc942c2c873b55e5080dc96
-- react-tween-state:
-- https://github.com/chenglou/react-tween-state/blob/master/index.js

cancelRaf :: RafHandle -> IO ()
cancelRaf = ffi "(function(id) { window.cancelAnimationFrame(id); })"

{-
-- The DOMHighResTimeStamp type is a double representing a number of
-- milliseconds, accurate to the thousandth of millisecond, that is with
-- a precision of 1 µs.
-}

render :: Elem -> (a -> React u ()) -> (a -> u -> a) -> a -> IO ()
render elem component update state = do
    stateRef <- newIORef state
    transitionRef <- newIORef []

    let updateCb update = modifyIORef transitionRef (update:)
        renderCb x = do
            prevState <- readIORef stateRef
            transitions <- readIORef transitionRef

            let newState = foldl update prevState transitions
                component' = component newState
            foreignNode <- runIdentity $ interpret component' updateCb
            render' elem foreignNode

            writeIORef stateRef newState
            writeIORef transitionRef []

            js_raf $ toPtr renderCb
            return ()

    renderCb 0
