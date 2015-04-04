{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module React.Events where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Maybe

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Prim
import GHCJS.Types

import React.Imports
import React.Types

-- XXX isn't this in GHCJS.Prim?
instance Eq JSString where
    (==) = eqRef

-- TODO: handle (a -> Maybe b) or (a -> b)

makeHandler :: RawAttrs -> (RawEvent -> Maybe (IO ()), EvtType) -> IO ()
makeHandler obj (handle, ChangeEvt) =
    makeHandler' obj handle js_set_onChange
makeHandler obj (handle, KeyDownEvt) =
    makeHandler' obj handle js_set_onKeyDown
makeHandler obj (handle, KeyPressEvt) =
    makeHandler' obj handle js_set_onKeyPress
makeHandler obj (handle, KeyUpEvt) =
    makeHandler' obj handle js_set_onKeyUp
makeHandler obj (handle, ClickEvt) =
    makeHandler' obj handle js_set_onClick
makeHandler obj (handle, DoubleClickEvt) =
    makeHandler' obj handle js_set_onDoubleClick
makeHandler obj (handle, MouseEnterEvt) =
    makeHandler' obj handle js_set_onMouseEnter
makeHandler obj (handle, MouseLeaveEvt) =
    makeHandler' obj handle js_set_onMouseLeave

handlerToJs :: (RawEvent -> Maybe (IO ())) -> IO (JSFun (RawEvent -> IO ()))
handlerToJs handle = syncCallback1 AlwaysRetain True $ \evt ->
    case handle evt of
        Nothing -> return ()
        Just x -> x


makeHandler' :: a
             -> (RawEvent -> Maybe (IO ()))
             -> (JSFun (RawEvent -> IO ()) -> a -> IO b)
             -> IO b
makeHandler' obj handle jsSet =
    join $ jsSet <$> handlerToJs handle <*> pure obj

unHandler :: (s -> IO ())
          -> EventHandler s
          -> (RawEvent -> Maybe (IO ()), EvtType)
unHandler act (EventHandler handle ty) = (\e -> act <$> handle e, ty)

onChange :: (ChangeEvent -> Maybe s) -> AttrOrHandler s
onChange = mkEventHandler ChangeEvt

onKeyDown :: (KeyboardEvent -> Maybe s) -> AttrOrHandler s
onKeyDown = mkEventHandler KeyDownEvt

onKeyPress :: (KeyboardEvent -> Maybe s) -> AttrOrHandler s
onKeyPress = mkEventHandler KeyPressEvt

onKeyUp :: (KeyboardEvent -> Maybe s) -> AttrOrHandler s
onKeyUp = mkEventHandler KeyUpEvt

onMouseEnter :: (MouseEvent -> Maybe s) -> AttrOrHandler s
onMouseEnter = mkEventHandler MouseEnterEvt

onMouseLeave :: (MouseEvent -> Maybe s) -> AttrOrHandler s
onMouseLeave = mkEventHandler MouseLeaveEvt

onDoubleClick :: (MouseEvent -> Maybe s) -> AttrOrHandler s
onDoubleClick = mkEventHandler DoubleClickEvt

onClick :: (MouseEvent -> Maybe s) -> AttrOrHandler s
onClick = mkEventHandler ClickEvt

onEnter :: s -> AttrOrHandler s
onEnter s = onKeyPress handler where
    handler KeyboardEvent{key="Enter"} = Just s
    handler _ = Nothing
