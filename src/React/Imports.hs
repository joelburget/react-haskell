{-# LANGUAGE CPP #-}

#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif

module React.Imports where

import GHCJS.DOM.Types (Element(..))
import GHCJS.Types

import React.Types

type Elem = Element

#ifdef __GHCJS__
foreign import javascript unsafe "React.render($1, $2)" js_render :: ForeignNode -> Elem -> IO ()
#else
js_render :: ForeignNode -> Elem -> IO ()
js_render = error "cannot evaluate js_render in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "BezierEasing($1, $2, $3, $4)($5)" js_bezier :: Double -> Double -> Double -> Double -> Double -> Double
#else
js_bezier :: Double -> Double -> Double -> Double -> Double -> Double
js_bezier = error "cannot evaluate js_bezier in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "React.createClass({ render: $1 })" js_createClass :: JSFun (IO (ReactT ty state sig)) -> IO ForeignClass
#else
js_createClass :: JSFun (IO (ReactT state sig))
               -> IO ForeignClass
js_createClass = error "cannot evaluate js_createClass in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "window.requestAnimationFrame($1)" js_raf :: JSFun (JSRef Double -> IO ()) -> IO RenderHandle
#else
js_raf :: JSFun (JSRef Double -> IO ()) -> IO RenderHandle
js_raf = error "cannot evaluate js_raf in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "window.cancelAnimationFrame($1);" js_cancelRaf :: RenderHandle -> IO ()
#else
js_cancelRaf :: RenderHandle -> IO ()
js_cancelRaf = error "cannot evaluate js_cancelRaf in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "React.DOM[$1]($2)" js_React_DOM_leaf :: JSString -> RawAttrs -> IO ForeignNode
#else
js_React_DOM_leaf :: JSString -> RawAttrs -> IO ForeignNode
js_React_DOM_leaf = error "cannot evaluate js_React_DOM_leaf in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "React.DOM[$1]($2, $3)" js_React_DOM_parent :: JSString -> RawAttrs -> ReactArray -> IO ForeignNode
#else
js_React_DOM_parent :: JSString -> RawAttrs -> ReactArray -> IO ForeignNode
js_React_DOM_parent = error "cannot evaluate js_React_DOM_parent in ghc"
#endif

#ifdef __GHCJS__
-- TODO(joel) - use newArray?
foreign import javascript unsafe "$r = [];" js_empty_arr :: IO RawAttrs
#else
js_empty_arr :: IO RawAttrs
js_empty_arr = error "cannot evaluate js_empty_arr in ghc"
#endif

#ifdef __GHCJS__
-- TODO(joel) - use newObj?
foreign import javascript unsafe "$r = {};" js_empty_object :: IO RawAttrs
#else
js_empty_object :: IO RawAttrs
js_empty_object = error "cannot evaluate js_empty_object in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$1[$2] = $3;" js_set_field_String :: RawAttrs -> JSString -> JSString -> IO ()
#else
js_set_field_String :: RawAttrs -> JSString -> JSString -> IO ()
js_set_field_String = error "cannot evaluate js_set_field_String in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$1[$2] = $3;" js_set_field_Double :: RawAttrs -> JSString -> Double -> IO ()
#else
js_set_field_Double :: RawAttrs -> JSString -> Double -> IO ()
js_set_field_Double = error "cannot evaluate js_set_field_Double in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$1[$2] = $3;" js_set_field_Int :: RawAttrs -> JSString -> Int -> IO ()
#else
js_set_field_Int :: RawAttrs -> JSString -> Int -> IO ()
js_set_field_Int = error "cannot evaluate js_set_field_Int in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$1[$2] = $3;" js_set_field_Obj :: RawAttrs -> JSString -> RawAttrs -> IO ()
#else
js_set_field_Obj :: RawAttrs -> JSString -> RawAttrs -> IO ()
js_set_field_Obj = error "cannot evaluate js_set_field_Obj in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$1[$2] = $3;" js_set_field_Arr :: RawAttrs -> JSString -> RawAttrs -> IO ()
#else
js_set_field_Arr :: RawAttrs -> JSString -> RawAttrs -> IO ()
js_set_field_Arr = error "cannot evaluate js_set_field_Arr in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$1[$2] = true;" js_set_field_True :: RawAttrs -> JSString -> IO ()
#else
js_set_field_True :: RawAttrs -> JSString -> IO ()
js_set_field_True = error "cannot evaluate js_set_field_True in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$1[$2] = false;" js_set_field_False :: RawAttrs -> JSString -> IO ()
#else
js_set_field_False :: RawAttrs -> JSString -> IO ()
js_set_field_False = error "cannot evaluate js_set_field_False in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$1[$2] = $3;" js_set_ix_Bool :: RawAttrs -> Int -> Bool -> IO ()
#else
js_set_ix_Bool :: RawAttrs -> Int -> Bool -> IO ()
js_set_ix_Bool = error "cannot evaluate js_set_ix_Bool in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$1[$2] = $3;" js_set_ix_Double :: RawAttrs -> Int -> Double -> IO ()
#else
js_set_ix_Double :: RawAttrs -> Int -> Double -> IO ()
js_set_ix_Double = error "cannot evaluate js_set_ix_Double in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$1[$2] = $3;" js_set_ix_String :: RawAttrs -> Int -> JSString -> IO ()
#else
js_set_ix_String :: RawAttrs -> Int -> JSString -> IO ()
js_set_ix_String = error "cannot evaluate js_set_ix_String in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$1[$2] = $3;" js_set_ix_Arr :: RawAttrs -> Int -> RawAttrs -> IO ()
#else
js_set_ix_Arr :: RawAttrs -> Int -> RawAttrs -> IO ()
js_set_ix_Arr = error "cannot evaluate js_set_ix_Arr in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$1[$2] = $3;" js_set_ix_Obj :: RawAttrs -> Int -> RawAttrs -> IO ()
#else
js_set_ix_Obj :: RawAttrs -> Int -> RawAttrs -> IO ()
js_set_ix_Obj = error "cannot evaluate js_set_ix_Obj in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "js_set_onClick" js_set_onClick       :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onClick       :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onClick       = error "cannot evaluate js_set_onClick in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "js_set_onDoubleClick" js_set_onDoubleClick :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onDoubleClick :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onDoubleClick = error "cannot evaluate js_set_onDoubleClick in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "js_set_onChange" js_set_onChange      :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onChange      :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onChange      = error "cannot evaluate js_set_onChangein ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "js_set_onKeyUp" js_set_onKeyUp       :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onKeyUp       :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onKeyUp       = error "cannot evaluate js_set_onKeyUp in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "js_set_onKeyPress" js_set_onKeyPress    :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onKeyPress    :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onKeyPress    = error "cannot evaluate js_set_onKeyPress in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "js_set_onKeyDown" js_set_onKeyDown     :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onKeyDown     :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onKeyDown     = error "cannot evaluate js_set_onKeyDown in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "js_set_onBlur" js_set_onBlur        :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onBlur        :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onBlur        = error "cannot evaluate js_set_onBlur in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "js_set_onMouseEnter" js_set_onMouseEnter :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onMouseEnter :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onMouseEnter = error "cannot evaluate js_set_onMouseEnter in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "js_set_onMouseLeave" js_set_onMouseLeave :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
#else
js_set_onMouseLeave :: JSFun (RawEvent -> IO ()) -> RawAttrs -> IO ()
js_set_onMouseLeave = error "cannot evaluate js_set_onMouseLeave in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "js_empty" js_ReactArray_empty :: IO ReactArray
#else
js_ReactArray_empty :: IO ReactArray
js_ReactArray_empty = error "cannot evaluate js_ReactArray_empty in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "js_push" js_ReactArray_push :: ReactArray -> ForeignNode -> IO ()
#else
js_ReactArray_push :: ReactArray -> ForeignNode -> IO ()
js_ReactArray_push = error "cannot evaluate js_ReactArray_push in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$r = $1" js_React_DOM_text :: JSString -> IO ForeignNode
#else
js_React_DOM_text :: JSString -> IO ForeignNode
js_React_DOM_text = error "cannot evaluate js_React_DOM_text in ghc"
#endif
