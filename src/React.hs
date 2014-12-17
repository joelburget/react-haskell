{-# LANGUAGE OverloadedStrings, FlexibleInstances, LambdaCase,
  MultiParamTypeClasses, FlexibleContexts, Rank2Types #-}

module React
    ( module X
    , getDomNode
    , render
    -- , nest
    -- , pureNest
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Identity
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
          => ReactT s m ()
          -> (s -> IO ())
          -> m (IO ForeignNode)
interpret react cb = do
    ~(child:_, ()) <- runReactT react
    return $ interpret' cb child

interpret' :: (s -> IO ())
           -> ReactNode s
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

locally :: Monad m
        => (local -> general)
        -> ReactT local m x
        -> ReactT general m x
locally localize nested = ReactT $ do
    (nodes, x) <- runReactT nested
    return (map (nodeConvert localize) nodes, x)

render' :: Elem -> ForeignNode -> IO ()
render' = ffi "(function(e,r){React.render(r,e);})"

render :: Elem -> (a -> React s ()) -> (a -> s -> a) -> a -> IO ()
render elem component update state = do
    let cb s = render elem component update (update state s)
        component' = component state
    foreignNode <- runIdentity $ interpret component' cb
    render' elem foreignNode
