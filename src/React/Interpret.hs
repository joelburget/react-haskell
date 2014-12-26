{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module React.Interpret where

import Control.Monad

import Haste.DOM
import Haste.Foreign
import Haste.JSON
import Haste.Prim

import React.Events
import React.Imports
import React.Types

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
          => ReactT anim signal m ()
          -> [RunningAnim signal]
          -> (signal -> IO ())
          -> m (IO ForeignNode)
interpret react anim cb = do
    ~(child:_, ()) <- runReactT react anim
    return $ interpret' cb child

interpret' :: (signal -> IO ())
           -> ReactNode signal
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
