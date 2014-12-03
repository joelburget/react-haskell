{-# LANGUAGE OverloadedStrings, FlexibleInstances, LambdaCase,
  MultiParamTypeClasses, FlexibleContexts #-}

module React
    ( module X
    , getDomNode
    , render
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

interpret :: Monad m
          => StatefulReactT s m ()
          -> s
          -> (s -> IO ())
          -> m (IO ForeignNode)
interpret react s cb = do
    ~(child:_, s', ()) <- runStatefulReactT react s
    return $ interpret' s' cb child

interpret' :: s
           -> (s -> IO ())
           -> ReactNode s
           -> IO ForeignNode
interpret' s cb = \case
    Parent name as hs children -> do
        children' <- forM children (interpret' s cb)
        let hs' = map (unStateful s cb) hs
        element js_React_DOM_parent name as hs' children'
    Leaf name as hs -> do
        let hs' = map (unStateful s cb) hs
        voidElement js_React_DOM_leaf name as hs'
    Text str -> js_React_DOM_text (toJSStr str)

unStateful :: s
           -> (s -> IO ())
           -> StatefulEventHandler s
           -> (RawEvent -> IO (), EvtType)
unStateful s act (StatefulEventHandler handle ty) = (act . handle s, ty)

element :: (JSString -> RawAttrs -> ReactArray -> IO ForeignNode)
        -> JSString
        -> Attrs
        -> [(RawEvent -> IO (), EvtType)]
        -> [ForeignNode]
        -> IO ForeignNode
element constructor name attrs handlers content = do
    attr <- js_empty_object
    mapM_ (setField attr) attrs
    mapM_ (makeHandler attr) handlers

    children <- js_ReactArray_empty
    mapM_ (js_ReactArray_push children) content
    constructor name attr children

makeHandler :: RawAttrs -> (RawEvent -> IO (), EvtType) -> IO ()
makeHandler obj (handle, ChangeEvt) = js_set_onChange (toPtr handle) obj
makeHandler obj (handle, KeyDownEvt) = js_set_onKeyDown (toPtr handle) obj
makeHandler obj (handle, KeyPressEvt) = js_set_onKeyPress (toPtr handle) obj
makeHandler obj (handle, KeyUpEvt) = js_set_onKeyUp (toPtr handle) obj
makeHandler obj (handle, ClickEvt) = js_set_onClick (toPtr handle) obj
makeHandler obj (handle, DoubleClickEvt) = js_set_onDoubleClick (toPtr handle) obj
makeHandler obj (handle, MouseEnterEvt) = js_set_onMouseEnter (toPtr handle) obj
makeHandler obj (handle, MouseLeaveEvt) = js_set_onMouseLeave (toPtr handle) obj

voidElement :: (JSString -> RawAttrs -> IO ForeignNode)
            -> JSString
            -> Attrs
            -> [(RawEvent -> IO (), EvtType)]
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

getDomNode :: ForeignNode -> IO (Maybe Elem)
getDomNode r = fmap fromPtr (js_React_getDomNode r)

render :: s -> Elem -> StatefulReact s () -> IO ()
render s elem r = do
    let cb s' = render s' elem r
    foreignNode <- runIdentity $ interpret r s cb
    render' elem foreignNode

render' :: Elem -> ForeignNode -> IO ()
render' = ffi (toJSStr "(function(e,r){React.render(r,e);})")
