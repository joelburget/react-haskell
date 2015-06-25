{-# LANGUAGE CPP #-}

#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#else
{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
#endif

#ifdef __GHCJS__
module React.Imports
    ( JSAny
    , RawEvent
    , js_render
    , js_createClass
    , js_react_createElement_DOM
    , js_react_createElement_Class
    , js_set_handler
    , js_forceUpdate
    ) where

-- newtype RawEvent = RawEvent JSAny
import React.GHCJS
#else
module React.Imports where

import React.GHCJS

data ForeignRetention
    = NeverRetain
    | AlwaysRetain
    | DomRetain (forall a. JSRef a)

castRef :: JSRef a -> JSRef b
castRef _ = JSRef

newObj :: IO (JSRef a)
newObj = undefined

eqRef :: JSRef a -> JSRef a -> Bool
eqRef = undefined

toArray :: [JSRef a] -> IO (JSArray a)
toArray = undefined

setProp :: ToJSString a => a -> JSRef b -> JSRef c -> IO ()
setProp = undefined

syncCallback1 :: ForeignRetention
              -> Bool
              -> (JSRef a -> IO b)
              -> IO (JSFun (JSRef a -> IO b))
syncCallback1 = undefined

syncCallback2 :: ForeignRetention
              -> Bool
              -> (JSRef a -> JSRef b -> IO c)
              -> IO (JSFun (JSRef a -> JSRef b -> IO c))
syncCallback2 = undefined
#endif

data RawEvent_
type RawEvent = JSRef RawEvent_
type JSAny = JSRef ()

#ifdef __GHCJS__
foreign import javascript unsafe "React.render($1, $2)"
    js_render :: JSRef () -> Element -> IO ()
foreign import javascript unsafe "js_createClass"
    js_createClass :: JSAny -> JSAny
foreign import javascript unsafe "React.createElement.apply(null, [$1, $2].concat($3))"
    js_react_createElement_DOM :: JSString -> JSAny -> JSAny -> IO JSAny
foreign import javascript unsafe "React.createElement.apply(null, [$1, $2].concat($3))"
-- foreign import javascript unsafe "function(x, y, z) { console.log(x, y, z, [x,y].concat(z)); return React.createElement.apply(null, [x, y].concat(z)); }($1, $2, $3)"
    js_react_createElement_Class :: JSAny -> JSAny -> JSAny -> IO JSAny
foreign import javascript unsafe "js_set_handler"
    js_set_handler :: Int -> JSString -> (JSFun (JSRef Int -> RawEvent -> IO ())) -> JSAny -> IO ()
foreign import javascript unsafe "$1.forceUpdate()"
    js_forceUpdate :: JSAny -> IO ()
#else
js_render :: JSRef () -> Element -> IO ()
js_render = error "cannot evaluate js_render in ghc"

js_createClass :: JSAny -> JSAny
js_createClass = error "cannot evaluate js_createClass in ghc"

js_react_createElement_DOM :: JSString -> JSAny -> JSAny -> IO JSAny
js_react_createElement_DOM = error "cannot evaluate js_react_createElement_DOM in ghc"

js_react_createElement_Class :: JSAny -> JSAny -> JSAny -> IO JSAny
js_react_createElement_Class = error "cannot evaluate js_react_createElement_Class in ghc"

js_set_handler :: Int -> JSString -> JSFun (JSRef Int -> RawEvent -> IO ()) -> JSAny -> IO ()
js_set_handler = error "cannot evaluate js_set_handler in ghc"

js_forceUpdate :: JSAny -> IO ()
js_forceUpdate = error "cannot evaluate js_forceUpdate in ghc"
#endif
