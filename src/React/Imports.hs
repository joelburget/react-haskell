{-# LANGUAGE ForeignFunctionInterface #-}
module React.Imports where

import React.Types
import React.Events

import Haste
import Haste.Foreign
import Haste.JSON
import Haste.Prim

foreign import ccall js_React_DOM_leaf :: JSString -> RawAttrs -> IO ForeignNode
foreign import ccall js_React_DOM_parent :: JSString -> RawAttrs -> ReactArray -> IO ForeignNode

foreign import ccall js_empty_object :: IO RawAttrs
foreign import ccall "js_set_field" js_set_field_String :: RawAttrs -> JSString -> JSString -> IO ()
foreign import ccall "js_set_field" js_set_field_Double :: RawAttrs -> JSString -> Double -> IO ()
foreign import ccall "js_set_field" js_set_field_Int :: RawAttrs -> JSString -> Int -> IO ()
foreign import ccall "js_set_field" js_set_field_Obj :: RawAttrs -> JSString -> RawAttrs -> IO ()
foreign import ccall js_set_field_True :: RawAttrs -> JSString -> IO ()
foreign import ccall js_set_field_False :: RawAttrs -> JSString -> IO ()
foreign import ccall js_targetValue :: RawChangeEvent -> JSString

foreign import ccall js_set_onClick       :: Ptr (RawMouseEvent -> IO ()) -> RawAttrs -> IO ()
foreign import ccall js_set_onDoubleClick :: Ptr (RawMouseEvent -> IO ()) -> RawAttrs -> IO ()
foreign import ccall js_set_onChange      :: Ptr (RawChangeEvent -> IO ()) -> RawAttrs -> IO ()
foreign import ccall js_set_onKeyUp       :: Ptr (RawKeyboardEvent -> IO ()) -> RawAttrs -> IO ()
foreign import ccall js_set_onKeyPress    :: Ptr (RawKeyboardEvent -> IO ()) -> RawAttrs -> IO ()
foreign import ccall js_set_onKeyDown     :: Ptr (RawKeyboardEvent -> IO ()) -> RawAttrs -> IO ()
foreign import ccall js_set_onBlur        :: Ptr (RawFocusEvent -> IO ()) -> RawAttrs -> IO ()

foreign import ccall js_React_getDomNode :: ForeignNode -> IO (Ptr (Maybe Elem))

foreign import ccall "js_empty" js_ReactArray_empty :: IO ReactArray
foreign import ccall "js_push" js_ReactArray_push :: ReactArray -> ForeignNode -> IO ()

foreign import ccall "js_id" js_React_DOM_text :: JSString -> IO ForeignNode

foreign import ccall js_parseChangeEvent :: RawChangeEvent -> Ptr ChangeEvent
foreign import ccall js_parseKeyboardEvent :: RawKeyboardEvent -> Ptr KeyboardEvent
foreign import ccall js_parseMouseEvent :: RawMouseEvent -> Ptr MouseEvent
