{-# LANGUAGE OverloadedStrings, FlexibleInstances, CPP, LambdaCase,
  MultiParamTypeClasses, FlexibleContexts #-}

module React
    ( module X
    , ReactAttrs(..)
    , defaultAttrs
    , ReactNode(..)
    , ReactM(..)

    , div
    , input
    , pre

    , getDomNode
    , renderComponent

    , (<!)
    , (<!?)

    , className

    , onChange
    , onKeyDown
    , onKeyPress
    , onKeyUp
    , onClick
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.String

#ifdef __HASTE__
import Haste hiding (fromString)
import Haste.Foreign
import Haste.JSON
import Haste.Prim
#endif

import Prelude hiding (div)

import React.Types as X
import React.Events as X
import React.Imports as X

-- TODO
-- * restricted monads
-- * store elem in monad
-- * store state in monad / provide better help
-- * provide alternative names for div, span, others?
-- * helpers for e.g. className

{-
class MonadReact m where

instance MonadReact ReactSansChildren where

instance MonadReact ReactWithChildren where

class ReactAttr a where
-}
data ReactAttrs = ReactAttrs
    { attrs :: [(JSString, JSON)]
    , handlers :: [EventHandler]
    }

instance Monoid ReactAttrs where
    mempty = ReactAttrs [] []
    (ReactAttrs a1 h1) `mappend` (ReactAttrs a2 h2) =
        ReactAttrs (a1 <> a2) (h1 <> h2)

defaultAttrs :: ReactAttrs
defaultAttrs = ReactAttrs [] []

data ReactNode = Div ReactAttrs [ReactNode]
               | Input ReactAttrs
               | Pre ReactAttrs [ReactNode] -- it'd be super cool to restrict this to a string somehow (restrict the underlying monad so it can only set attrs and string?)
               | Text ReactAttrs String

data ReactM a = ReactM ReactAttrs [ReactNode] a

instance Functor ReactM where
    f `fmap` (ReactM attrs nodes a) = ReactM attrs nodes (f a)

instance Applicative ReactM where
    pure = ReactM defaultAttrs []
    (ReactM af nf f) <*> (ReactM aa na a) = ReactM (af <> aa) (nf <> na) (f a)

instance Monad ReactM where
    return = pure
    (ReactM aa na a) >>= nf =
        let ReactM as ns a' = nf a
        in ReactM (aa <> as) (na <> ns) a'

instance IsString (ReactM a) where
    fromString str = ReactM defaultAttrs [Text defaultAttrs str] undefined

class Attributable h a where
    (<!) :: h -> a -> h

(<!?) :: Attributable h a => h -> (Bool, a) -> h
h <!? (True, a) = h <! a
h <!? (False, _) = h

instance Attributable (ReactM b) (JSString, JSON) where
    (ReactM (ReactAttrs as hs) cs x) <! attr =
        ReactM (ReactAttrs (attr:as) hs) cs x

instance Attributable (ReactM b) EventHandler where
    (ReactM (ReactAttrs as hs) cs x) <! hndl =
        ReactM (ReactAttrs as (hndl:hs)) cs x

instance Attributable (ReactM c) a =>
         Attributable (ReactM b -> ReactM c) a where
    f <! attr = (<! attr) . f

className :: JSString -> (JSString, JSON)
className str = ("className", Str str)

div :: ReactM () -> ReactM ()
div (ReactM attrs children _) = ReactM defaultAttrs [Div attrs children] ()

pre :: ReactM () -> ReactM ()
pre (ReactM attrs children _) = ReactM defaultAttrs [Pre attrs children] ()

input :: ReactM () -> ReactM ()
input (ReactM attrs children _) = ReactM defaultAttrs [Input attrs] ()

getDomNode :: React -> IO (Maybe Elem)
getDomNode r = fmap fromPtr (js_React_getDomNode r)

interpretReact :: ReactM () -> IO React
-- TODO we really shouldn't just take the first element here - invariants
-- must be maintained
interpretReact rm = head <$> interpretReact' rm

interpretReact' :: ReactM () -> IO [React]
interpretReact' (ReactM _ nodes _) = forM nodes $ \case
    Div attrs children -> do
        children' <- interpretReact' (ReactM defaultAttrs children ())
        element js_React_DOM_div attrs children'
    Input attrs -> voidElement js_React_DOM_input attrs
    Text attrs str -> js_React_DOM_text (toJSStr str)
    Pre attrs children -> do
        children' <- interpretReact' (ReactM defaultAttrs children ())
        element js_React_DOM_pre attrs children'

element :: (RawAttrs -> ReactArray -> IO React)
        -> ReactAttrs
        -> [React]
        -> IO React
element constructor (ReactAttrs attrs handlers) content = do
    attr <- js_empty_object
    mapM_ (setField attr) attrs
    mapM_ (($ attr) . unEventHandler) handlers

    children <- js_ReactArray_empty
    mapM_ (js_ReactArray_push children) content
    constructor attr children

voidElement :: (RawAttrs -> IO React)
            -> ReactAttrs
            -> IO React
voidElement constructor reactAttrs =
    element (\a c -> constructor a) reactAttrs []

setField :: RawAttrs -> (JSString, JSON) -> IO ()
setField attr (fld, Str v) = js_set_field_String attr fld v
setField attr (fld, Dict vs) = do
    subObj <- js_empty_object
    mapM_ (setField subObj) vs
    js_set_field_Obj attr fld subObj
setField attr (fld, Num v) = js_set_field_Double attr fld v
setField attr (fld, Bool True) = js_set_field_True attr fld
setField attr (fld, Bool False) = js_set_field_False attr fld

-- TODO this seems wrong
setField attr (fld, Null) = return ()

renderComponent :: Elem -> ReactM () -> IO ()
renderComponent elem r = do
    r' <- interpretReact r
    renderComponent' elem r'

renderComponent' :: Elem -> React -> IO ()
renderComponent' = ffi (toJSStr "(function(e,r){React.renderComponent(r,e);})")

-- newtype RawAttrs = RawAttrs JSAny  deriving (Pack, Unpack)
-- EventHandler :: (RawAttrs -> IO (}) -> EventHandler
-- js_set_onChange :: Ptr (RawChangeEvent -> IO ()) -> RawAttrs -> IO ()

makeHandler :: EventHandler -> ReactM ()
makeHandler handler = ReactM (ReactAttrs [] [handler]) [] ()

onChange :: (ChangeEvent -> IO ()) -> ReactM ()
onChange = makeHandler . onChange'

onChange' :: (ChangeEvent -> IO ()) -> EventHandler
onChange' cb = EventHandler $ js_set_onChange $ toPtr $
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


-- targetValue :: RawChangeEvent -> IO JSString
-- targetValue = ffi "(function(event) { return event.target.value; })"
