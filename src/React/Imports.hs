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
foreign import javascript unsafe "React.render($1, $2)" js_render :: JSRef () -> Elem -> IO ()
#else
js_render :: JSRef () -> Elem -> IO ()
js_render = error "cannot evaluate js_render in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "js_createClass" js_createClass :: JSRef JSAny -> JSAny
#else
js_createClass :: JSRef JSAny -> JSAny
js_createClass = error "cannot evaluate js_createClass in ghc"
#endif
