module React.Events where

import Haste
import Haste.Prim

import React.Imports
import React.Types

onChange :: (s -> ChangeEvent -> s) -> StatefulEventHandler s
onChange handler = StatefulEventHandler
    (\s evt -> handler s (fromPtr (js_parseChangeEvent evt)))
    ChangeEvt

onKeyDown :: (s -> KeyboardEvent -> s) -> StatefulEventHandler s
onKeyDown handler = StatefulEventHandler
    (\s evt -> handler s (fromPtr (js_parseKeyboardEvent evt)))
    KeyDownEvt

onKeyPress :: (s -> KeyboardEvent -> s) -> StatefulEventHandler s
onKeyPress handler = StatefulEventHandler
    (\s evt -> handler s (fromPtr (js_parseKeyboardEvent evt)))
    KeyPressEvt

onKeyUp :: (s -> KeyboardEvent -> s) -> StatefulEventHandler s
onKeyUp handler = StatefulEventHandler
    (\s evt -> handler s (fromPtr (js_parseKeyboardEvent evt)))
    KeyUpEvt

onClick :: (s -> MouseEvent -> s) -> StatefulEventHandler s
onClick handler = StatefulEventHandler
    (\s evt -> handler s (fromPtr (js_parseMouseEvent evt)))
    ClickEvt
