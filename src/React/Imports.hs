{-# LANGUAGE CPP #-}

#ifdef __HASTE__
{-# LANGUAGE ForeignFunctionInterface #-}
#endif

module React.Imports where

import React.Types

import Haste
import Haste.Foreign
import Haste.JSON
import Haste.Prim

#ifdef __HASTE__
foreign import ccall js_overState:: Ptr ForeignClassInstance -> Ptr (Ptr state -> Ptr state) -> IO ()
#else
js_overState:: Ptr ForeignClassInstance -> Ptr (Ptr state -> Ptr state) -> IO ()
js_overState = error "cannot evaluate js_overState in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_render :: ForeignClass -> Elem -> IO ()
#else
js_render :: ForeignClass -> Elem -> IO ()
js_render = error "cannot evaluate js_render in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_bezier :: Double -> Double -> Double -> Double -> Double -> Double
#else
js_bezier :: Double -> Double -> Double -> Double -> Double -> Double
js_bezier = error "cannot evaluate js_bezier in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_createClass :: Ptr (Ptr ForeignClassInstance -> Ptr state -> IO ForeignNode)
                                    -> Ptr state
                                    -> IO ForeignClass
#else
js_createClass :: Ptr (Ptr ForeignClassInstance -> Ptr state -> ForeignNode)
               -> Ptr state
               -> IO ForeignClass
js_createClass = error "cannot evaluate js_createClass in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_raf :: Ptr (Double -> IO ()) -> IO RenderHandle
#else
js_raf :: Ptr (Double -> IO ()) -> IO RenderHandle
js_raf = error "cannot evaluate js_raf in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_cancelRaf :: RenderHandle -> IO ()
#else
js_cancelRaf :: RenderHandle -> IO ()
js_cancelRaf = error "cannot evaluate js_cancelRaf in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_React_DOM_leaf :: JSString -> RawAttrs -> IO ForeignNode
#else
js_React_DOM_leaf :: JSString -> RawAttrs -> IO ForeignNode
js_React_DOM_leaf = error "cannot evaluate js_React_DOM_leaf in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_React_DOM_parent :: JSString -> RawAttrs -> ReactArray -> IO ForeignNode
#else
js_React_DOM_parent :: JSString -> RawAttrs -> ReactArray -> IO ForeignNode
js_React_DOM_parent = error "cannot evaluate js_React_DOM_parent in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_React_DOM_class :: ForeignClass -> IO ForeignNode
#else
js_React_DOM_class :: ForeignClass -> IO ForeignNode
js_React_DOM_class = error "cannot evaluate js_React_DOM_class in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_empty_arr :: IO RawAttrs
#else
js_empty_arr :: IO RawAttrs
js_empty_arr = error "cannot evaluate js_empty_arr in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_empty_object :: IO RawAttrs
#else
js_empty_object :: IO RawAttrs
js_empty_object = error "cannot evaluate js_empty_object in ghc"
#endif

#ifdef __HASTE__
foreign import ccall "js_set_field" js_set_field_String :: RawAttrs -> JSString -> JSString -> IO ()
#else
js_set_field_String :: RawAttrs -> JSString -> JSString -> IO ()
js_set_field_String = error "cannot evaluate js_set_field_String in ghc"
#endif

#ifdef __HASTE__
foreign import ccall "js_set_field" js_set_field_Double :: RawAttrs -> JSString -> Double -> IO ()
#else
js_set_field_Double :: RawAttrs -> JSString -> Double -> IO ()
js_set_field_Double = error "cannot evaluate js_set_field_Double in ghc"
#endif

#ifdef __HASTE__
foreign import ccall "js_set_field" js_set_field_Int :: RawAttrs -> JSString -> Int -> IO ()
#else
js_set_field_Int :: RawAttrs -> JSString -> Int -> IO ()
js_set_field_Int = error "cannot evaluate js_set_field_Int in ghc"
#endif

#ifdef __HASTE__
foreign import ccall "js_set_field" js_set_field_Obj :: RawAttrs -> JSString -> RawAttrs -> IO ()
#else
js_set_field_Obj :: RawAttrs -> JSString -> RawAttrs -> IO ()
js_set_field_Obj = error "cannot evaluate js_set_field_Obj in ghc"
#endif

#ifdef __HASTE__
foreign import ccall "js_set_field" js_set_field_Arr :: RawAttrs -> JSString -> RawAttrs -> IO ()
#else
js_set_field_Arr :: RawAttrs -> JSString -> RawAttrs -> IO ()
js_set_field_Arr = error "cannot evaluate js_set_field_Arr in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_set_field_True :: RawAttrs -> JSString -> IO ()
#else
js_set_field_True :: RawAttrs -> JSString -> IO ()
js_set_field_True = error "cannot evaluate js_set_field_True in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_set_field_False :: RawAttrs -> JSString -> IO ()
#else
js_set_field_False :: RawAttrs -> JSString -> IO ()
js_set_field_False = error "cannot evaluate js_set_field_False in ghc"
#endif

#ifdef __HASTE__
foreign import ccall "js_set_field" js_set_ix_Bool :: RawAttrs -> Int -> Bool -> IO ()
#else
js_set_ix_Bool :: RawAttrs -> Int -> Bool -> IO ()
js_set_ix_Bool = error "cannot evaluate js_set_ix_Bool in ghc"
#endif

#ifdef __HASTE__
foreign import ccall "js_set_field" js_set_ix_Double :: RawAttrs -> Int -> Double -> IO ()
#else
js_set_ix_Double :: RawAttrs -> Int -> Double -> IO ()
js_set_ix_Double = error "cannot evaluate js_set_ix_Double in ghc"
#endif

#ifdef __HASTE__
foreign import ccall "js_set_field" js_set_ix_String :: RawAttrs -> Int -> JSString -> IO ()
#else
js_set_ix_String :: RawAttrs -> Int -> JSString -> IO ()
js_set_ix_String = error "cannot evaluate js_set_ix_String in ghc"
#endif

#ifdef __HASTE__
foreign import ccall "js_set_field" js_set_ix_Arr :: RawAttrs -> Int -> RawAttrs -> IO ()
#else
js_set_ix_Arr :: RawAttrs -> Int -> RawAttrs -> IO ()
js_set_ix_Arr = error "cannot evaluate js_set_ix_Arr in ghc"
#endif

#ifdef __HASTE__
foreign import ccall "js_set_field" js_set_ix_Obj :: RawAttrs -> Int -> RawAttrs -> IO ()
#else
js_set_ix_Obj :: RawAttrs -> Int -> RawAttrs -> IO ()
js_set_ix_Obj = error "cannot evaluate js_set_ix_Obj in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_targetValue :: RawEvent -> JSString
#else
js_targetValue :: RawEvent -> JSString
js_targetValue = error "cannot evaluate js_targetValue in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_set_onClick       :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onClick       :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onClick       = error "cannot evaluate js_set_onClick in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_set_onDoubleClick :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onDoubleClick :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onDoubleClick = error "cannot evaluate js_set_onDoubleClick in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_set_onChange      :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onChange      :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onChange      = error "cannot evaluate js_set_onChangein ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_set_onKeyUp       :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onKeyUp       :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onKeyUp       = error "cannot evaluate js_set_onKeyUp in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_set_onKeyPress    :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onKeyPress    :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onKeyPress    = error "cannot evaluate js_set_onKeyPress in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_set_onKeyDown     :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onKeyDown     :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onKeyDown     = error "cannot evaluate js_set_onKeyDown in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_set_onBlur        :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onBlur        :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onBlur        = error "cannot evaluate js_set_onBlur in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_set_onMouseEnter :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onMouseEnter :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onMouseEnter = error "cannot evaluate js_set_onMouseEnter in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_set_onMouseLeave :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onMouseLeave :: Ptr (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onMouseLeave = error "cannot evaluate js_set_onMouseLeave in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_React_getDomNode :: ForeignNode -> IO (Ptr (Maybe Elem))
#else
js_React_getDomNode :: ForeignNode -> IO (Ptr (Maybe Elem))
js_React_getDomNode = error "cannot evaluate js_React_getDomNode in ghc"
#endif

#ifdef __HASTE__
foreign import ccall "js_empty" js_ReactArray_empty :: IO ReactArray
#else
js_ReactArray_empty :: IO ReactArray
js_ReactArray_empty = error "cannot evaluate js_ReactArray_empty in ghc"
#endif

#ifdef __HASTE__
foreign import ccall "js_push" js_ReactArray_push :: ReactArray -> ForeignNode -> IO ()
#else
js_ReactArray_push :: ReactArray -> ForeignNode -> IO ()
js_ReactArray_push = error "cannot evaluate js_ReactArray_push in ghc"
#endif

#ifdef __HASTE__
foreign import ccall "js_id" js_React_DOM_text :: JSString -> IO ForeignNode
#else
js_React_DOM_text :: JSString -> IO ForeignNode
js_React_DOM_text = error "cannot evaluate js_React_DOM_text in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_parseChangeEvent :: RawEvent -> Ptr ChangeEvent
#else
js_parseChangeEvent :: RawEvent -> Ptr ChangeEvent
js_parseChangeEvent = error "cannot evaluate js_parseChangeEvent in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_parseKeyboardEvent :: RawEvent -> Ptr KeyboardEvent
#else
js_parseKeyboardEvent :: RawEvent -> Ptr KeyboardEvent
js_parseKeyboardEvent = error "cannot evaluate js_parseKeyboardEvent in ghc"
#endif

#ifdef __HASTE__
foreign import ccall js_parseMouseEvent :: RawEvent -> Ptr MouseEvent
#else
js_parseMouseEvent :: RawEvent -> Ptr MouseEvent
js_parseMouseEvent = error "cannot evaluate js_parseMouseEvent in ghc"
#endif
