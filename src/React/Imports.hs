{-# LANGUAGE CPP #-}

#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif

module React.Imports where

import GHCJS.DOM.Types (Element(..))
import GHCJS.Types

type JSAny = JSRef ()
type Elem = Element

-- newtype RawEvent = RawEvent JSAny
data RawEvent_
type RawEvent = JSRef RawEvent_

#ifdef __GHCJS__
foreign import javascript unsafe "React.render($1, $2)"
    js_render :: JSRef () -> Elem -> IO ()
foreign import javascript unsafe "js_createClass"
    js_createClass :: JSAny -> JSAny
foreign import javascript unsafe "React.createElement.apply(null, [$1, $2].concat($3))"
    js_react_createElement_DOM :: JSString -> JSAny -> JSAny -> IO JSAny
foreign import javascript unsafe "React.createElement.apply(null, [$1, $2].concat($3))"
    js_react_createElement_Class :: JSAny -> JSAny -> JSAny -> IO JSAny
foreign import javascript unsafe "js_set_handler"
    js_set_handler :: JSString -> (JSFun (RawEvent -> IO ())) -> JSAny -> IO ()
foreign import javascript unsafe "this.componentId"
    js_componentId :: IO Int
#else
js_render :: JSRef () -> Elem -> IO ()
js_render = error "cannot evaluate js_render in ghc"

js_createClass :: JSAny -> JSAny
js_createClass = error "cannot evaluate js_createClass in ghc"

js_react_createElement_DOM :: JSString -> JSAny -> JSAny -> IO JSAny
js_react_createElement_DOM = error "cannot evaluate js_react_createElement_DOM in ghc"

js_react_createElement_Class :: JSAny -> JSAny -> JSAny -> IO JSAny
js_react_createElement_Class = error "cannot evaluate js_react_createElement_Class in ghc"

js_set_handler :: JSString -> (JSFun (RawEvent -> IO ())) -> JSAny -> IO ()
js_set_handler = error "cannot evaluate js_set_handler in ghc"
#endif
