{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module React.Interpret where

import Control.Monad

import GHCJS.Prim
import GHCJS.Types

import React.Events
import React.Imports
import React.Types


element :: (RawAttrs -> ReactArray -> IO ForeignNode)
        -> Attrs
        -> [(RawEvent -> Maybe (IO ()), EvtType)]
        -> [ForeignNode]
        -> IO ForeignNode
element constructor attrs handlers content = do
    attr <- js_empty_object
    mapM_ (setField attr) attrs
    mapM_ (makeHandler attr) handlers

    children <- js_ReactArray_empty
    mapM_ (js_ReactArray_push children) content
    constructor attr children


setField :: RawAttrs -> (JSString, JSON) -> IO ()
setField attr (fld, Num v) = js_set_field_Double attr fld v
setField attr (fld, Str v) = js_set_field_String attr fld v
setField attr (fld, Bool True) = js_set_field_True attr fld
setField attr (fld, Bool False) = js_set_field_False attr fld
setField attr (fld, Arr arr) = do
    jsArr <- js_empty_arr
    let arr' = zip [0..] arr
    mapM_ (uncurry (setIx jsArr)) arr'
    js_set_field_Arr attr fld jsArr
setField attr (fld, Dict vs) = do
    subObj <- js_empty_object
    mapM_ (setField subObj) vs
    js_set_field_Obj attr fld subObj

-- TODO this seems wrong
setField attr (fld, Null) = return ()


setIx :: RawAttrs -> Int -> JSON -> IO ()
setIx arr i (Num v) = js_set_ix_Double arr i v
setIx arr i (Str v) = js_set_ix_String arr i v
setIx arr i (Bool v) = js_set_ix_Bool arr i v
setIx arr i (Arr setArr) = do
    jsArr <- js_empty_arr
    let setArr' = zip [0..] setArr
    mapM_ (uncurry (setIx jsArr)) setArr'
    js_set_ix_Arr arr i jsArr
setIx arr i (Dict d) = do
    subObj <- js_empty_object
    mapM_ (setField subObj) d
    js_set_ix_Obj arr i subObj

-- TODO
setIx arr i Null = return ()


interpret :: React ty sig
          -> (sig -> IO ())
          -> IO ForeignNode
interpret react cb =
    -- TODO should be able to avoid this weird pattern match by not
    -- interpreting sequences!
    let child:_ = runReact react
    in interpret' cb child


interpret' :: (signal -> IO ())
           -> Child signal
           -> IO ForeignNode
interpret' cb = \case
    Static node -> interpret'' cb node
    Dynamic nodes -> do
        -- TODO this is all really gross. Have the imported functions operate
        -- directly on JSRefs, not RawAttrs.
        arr@(RawAttrs arr') <- js_empty_arr
        forM_ nodes $ \(i, node) -> do
            ForeignNode node' <- interpret'' cb node
            js_set_ix_Arr arr i (RawAttrs node')
        return (ForeignNode (castRef arr'))

interpret'' :: (signal -> IO ())
            -> ReactNode signal
            -> IO ForeignNode
interpret'' cb = \case
    Parent f as hs children -> do
        children' <- forM children (interpret' cb)
        let hs' = map (unHandler cb) hs
        element f as hs' children'
    Leaf f as hs -> do
        let hs' = map (unHandler cb) hs
        element f as hs' []
    Text str -> return (ForeignNode (castRef (toJSString str)))
