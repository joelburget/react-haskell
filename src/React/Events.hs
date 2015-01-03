{-# LANGUAGE OverloadedStrings #-}
module React.Events where

import Control.Applicative
import Control.DeepSeq

import Haste
import Haste.Prim

import React.Imports
import React.Types

-- TODO: handle (a -> Maybe b) or (a -> b)

handlerToJs :: (RawEvent -> Maybe (IO ())) -> Ptr (RawEvent -> IO ())
handlerToJs handle =
    let go :: Maybe (IO ()) -> IO ()
        go = maybe (return ()) id
    in toPtr (go . handle)

makeHandler :: RawAttrs -> (RawEvent -> Maybe (IO ()), EvtType) -> IO ()
makeHandler obj (handle, ChangeEvt) =
    js_set_onChange (handlerToJs handle) obj
makeHandler obj (handle, KeyDownEvt) =
    js_set_onKeyDown (handlerToJs handle) obj
makeHandler obj (handle, KeyPressEvt) =
    js_set_onKeyPress (handlerToJs handle) obj
makeHandler obj (handle, KeyUpEvt) =
    js_set_onKeyUp (handlerToJs handle) obj
makeHandler obj (handle, ClickEvt) =
    js_set_onClick (handlerToJs handle) obj
makeHandler obj (handle, DoubleClickEvt) =
    js_set_onDoubleClick (handlerToJs handle) obj
makeHandler obj (handle, MouseEnterEvt) =
    js_set_onMouseEnter (handlerToJs handle) obj
makeHandler obj (handle, MouseLeaveEvt) =
    js_set_onMouseLeave (handlerToJs handle) obj

mkEventHandler :: NFData s
               => (RawEvent -> s)
               -> EvtType
               -> (s -> Maybe s')
               -> EventHandler s'
mkEventHandler unNative ty handle =
    -- important - you must deepseq the event immediately - otherwise
    -- react's pooling will collect and destroy it.
    let handle' raw = handle $!! unNative raw
    in EventHandler handle' ty

unHandler :: (s -> IO ())
          -> EventHandler s
          -> (RawEvent -> Maybe (IO ()), EvtType)
unHandler act (EventHandler handle ty) = (\e -> act <$> handle e, ty)

onChange :: (ChangeEvent -> Maybe s) -> EventHandler s
onChange = mkEventHandler
    (fromPtr . js_parseChangeEvent)
    ChangeEvt

onKeyDown :: (KeyboardEvent -> Maybe s) -> EventHandler s
onKeyDown = mkEventHandler
    (fromPtr . js_parseKeyboardEvent)
    KeyDownEvt

onKeyPress :: (KeyboardEvent -> Maybe s) -> EventHandler s
onKeyPress = mkEventHandler
    (fromPtr . js_parseKeyboardEvent)
    KeyPressEvt

onKeyUp :: (KeyboardEvent -> Maybe s) -> EventHandler s
onKeyUp = mkEventHandler
    (fromPtr . js_parseKeyboardEvent)
    KeyUpEvt

onMouseEnter :: (MouseEvent -> Maybe s) -> EventHandler s
onMouseEnter = mkEventHandler
    (fromPtr . js_parseMouseEvent)
    MouseEnterEvt

onMouseLeave :: (MouseEvent -> Maybe s) -> EventHandler s
onMouseLeave = mkEventHandler
    (fromPtr . js_parseMouseEvent)
    MouseLeaveEvt

onDoubleClick :: (MouseEvent -> Maybe s) -> EventHandler s
onDoubleClick = mkEventHandler
    (fromPtr . js_parseMouseEvent)
    DoubleClickEvt

onClick :: (MouseEvent -> Maybe s) -> EventHandler s
onClick = mkEventHandler
    (fromPtr . js_parseMouseEvent)
    ClickEvt

onEnter :: s -> EventHandler s
onEnter s = onKeyPress handler where
    handler KeyboardEvent{key="Enter"} = Just s
    handler _ = Nothing
