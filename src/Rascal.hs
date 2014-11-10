{-# LANGUAGE OverloadedStrings, FlexibleInstances, CPP #-}

module Rascal
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

import Rascal.Types as X
import Rascal.Events as X
import Rascal.Imports as X

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

defaultAttrs :: ReactAttrs
defaultAttrs = ReactAttrs [] []

data ReactNode = Div ReactAttrs [ReactNode]
               | Input ReactAttrs
               | ReactStr String
               | Pre ReactAttrs String

data ReactM a = ReactM [ReactNode] a

instance Functor ReactM where
    f `fmap` (ReactM nodes a) = ReactM nodes (f a)

instance Applicative ReactM where
    pure = ReactM []
    (ReactM nf f) <*> (ReactM na a) = ReactM (nf <> na) (f a)

instance Monad ReactM where
    return = pure
    (ReactM na a) >>= nf = let ReactM ns a' = nf a in ReactM (na <> ns) a'

instance IsString (ReactM a) where
    fromString str = ReactM [ReactStr str] undefined

div :: ReactAttrs -> ReactM () -> ReactM ()
div attrs (ReactM children _) = ReactM [Div attrs children] ()

pre :: ReactAttrs -> String -> ReactM ()
pre attrs str = ReactM [Pre attrs str] ()

input :: ReactAttrs -> ReactM ()
input attrs = ReactM [Input attrs] ()

getDomNode :: React -> IO (Maybe Elem)
getDomNode r = fmap fromPtr (js_React_getDomNode r)

interpretReact :: ReactM () -> IO React
-- TODO we really shouldn't just take the first element here - invariants
-- must be maintained
interpretReact rm = head <$> interpretReact' rm

interpretReact' :: ReactM () -> IO [React]
interpretReact' (ReactM nodes _) = mapM
    (\node -> case node of
        Div attrs children -> do
            children' <- interpretReact' (ReactM children ())
            element js_React_DOM_div attrs children'
        Input attrs -> voidElement js_React_DOM_input attrs
        ReactStr str -> js_React_DOM_text (toJSStr str)
        Pre attrs str -> do
            children <- interpretReact' (ReactM [ReactStr str] ())
            element js_React_DOM_pre attrs children)
    nodes

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

{-
onClick :: (RawMouseEvent -> IO ()) -> EventHandler
onClick = EventHandler . js_set_onClick . toPtr
-}

-- newtype RawAttrs = RawAttrs JSAny  deriving (Pack, Unpack)
-- EventHandler :: (RawAttrs -> IO (}) -> EventHandler
-- js_set_onChange :: Ptr (RawChangeEvent -> IO ()) -> RawAttrs -> IO ()

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


-- targetValue :: RawChangeEvent -> IO JSString
-- targetValue = ffi "(function(event) { return event.target.value; })"
