module React.Events where

import Haste
import Haste.Prim

import React.Imports
import React.Types

onChange :: (ChangeEvent -> IO ()) -> EventHandler
onChange cb = EventHandler $ js_set_onChange $ toPtr $
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
