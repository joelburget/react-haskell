{-# LANGUAGE OverloadedStrings, FlexibleInstances, LambdaCase,
  MultiParamTypeClasses, FlexibleContexts #-}

module React
    ( module X
    , ReactNode(..)
    , ReactM(..)

    , getDomNode
    , render

    , className

    , onChange
    , onKeyDown
    , onKeyPress
    , onKeyUp
    , onClick
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.String

import Haste hiding (fromString)
import Haste.Foreign
import Haste.JSON
import Haste.Prim

import Prelude hiding (div)

import React.Attrs as X
import React.Elements as X
import React.Events as X
import React.Imports as X
import React.Types as X

-- TODO
-- * restricted monads
-- * store elem in monad
-- * store state in monad / provide better help
-- * provide alternative names for div, span, others?
-- * helpers for e.g. className
-- * rename away from "React"

{-
class MonadReact m where

instance MonadReact ReactSansChildren where

instance MonadReact ReactWithChildren where

class ReactAttr a where
-}

interpret :: React -> IO ForeignNode
interpret (ReactM _ _ (node:_) _) = interpret' node

interpret' :: ReactNode -> IO ForeignNode
interpret' = \case
    Parent name as hs children -> element js_React_DOM_parent name as hs =<< forM children interpret'
    Leaf name as hs -> voidElement js_React_DOM_leaf name as hs
    Text str -> js_React_DOM_text (toJSStr str)

element :: (JSString -> RawAttrs -> ReactArray -> IO ForeignNode)
        -> JSString
        -> Attrs
        -> Handlers
        -> [ForeignNode]
        -> IO ForeignNode
element constructor name attrs handlers content = do
    attr <- js_empty_object
    mapM_ (setField attr) attrs
    mapM_ (($ attr) . unEventHandler) handlers

    children <- js_ReactArray_empty
    mapM_ (js_ReactArray_push children) content
    constructor name attr children

voidElement :: (JSString -> RawAttrs -> IO ForeignNode)
            -> JSString
            -> Attrs
            -> Handlers
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

render :: Elem -> React -> IO ()
render elem r = do
    r' <- interpret r
    render' elem r'

render' :: Elem -> ForeignNode -> IO ()
render' = ffi (toJSStr "(function(e,r){React.render(r,e);})")

-- newtype RawAttrs = RawAttrs JSAny  deriving (Pack, Unpack)
-- EventHandler :: (RawAttrs -> IO (}) -> EventHandler
-- js_set_onChange :: Ptr (RawChangeEvent -> IO ()) -> RawAttrs -> IO ()

makeHandler :: EventHandler -> React
makeHandler handler = ReactM [] [handler] [] ()

onChange :: (ChangeEvent -> IO ()) -> React
onChange = makeHandler . onChange'

onChange' :: (ChangeEvent -> IO ()) -> EventHandler
onChange' cb = EventHandler $ js_set_onChange $ toPtr $
    cb . fromPtr . js_parseChangeEvent

onKeyDown :: (KeyboardEvent -> IO ()) -> EventHandler
onKeyDown cb = EventHandler $ js_set_onKeyDown $ toPtr $
    cb . fromPtr . js_parseKeyboardEvent

onKeyPress :: (KeyboardEvent -> IO ()) -> EventHandler
onKeyPress cb = EventHandler $ js_set_onKeyPress $ toPtr $
    cb . fromPtr . js_parseKeyboardEvent

onKeyUp :: (KeyboardEvent -> IO ()) -> EventHandler
onKeyUp cb = EventHandler $ js_set_onKeyUp $ toPtr $
    cb . fromPtr . js_parseKeyboardEvent

onClick :: (MouseEvent -> IO ()) -> EventHandler
onClick cb = EventHandler $ js_set_onClick $ toPtr $
    cb . fromPtr . js_parseMouseEvent


-- targetValue :: RawChangeEvent -> IO JSString
-- targetValue = ffi "(function(event) { return event.target.value; })"
