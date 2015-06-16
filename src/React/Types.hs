{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, TypeFamilies,
    ExistentialQuantification, ImpredicativeTypes, LiberalTypeSynonyms,
    DeriveGeneric, DataKinds, GADTs, OverloadedStrings, CPP #-}

module React.Types where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.List (partition)
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)
import GHC.Generics
import System.IO.Unsafe

import qualified Data.Aeson as Aeson
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import React.Imports

import Debug.Trace

instance Show JSString where
    show = fromJSString

type JSON = Aeson.Value

-- instance IsString JSON where
--   fromString = Str . fromString

newtype ForeignNode = ForeignNode JSAny
newtype RawAttrs = RawAttrs JSAny
newtype ReactArray = ReactArray JSAny
newtype ForeignClass = ForeignClass JSAny
type ForeignRender = RawAttrs -> ReactArray -> IO ForeignNode

newtype RenderHandle = RenderHandle Int

data EvtType
   = ChangeEvt
   | KeyDownEvt
   | KeyPressEvt
   | KeyUpEvt
   | ClickEvt
   | DoubleClickEvt
   | MouseEnterEvt
   | MouseLeaveEvt


jsName :: EvtType -> JSString
jsName ChangeEvt = "onChange"
jsName KeyDownEvt = "onKeyDown"
jsName KeyPressEvt = "onKeyPress"
jsName KeyUpEvt = "onKeyUp"
jsName ClickEvt = "onClick"
jsName DoubleClickEvt = "onDoubleClick"
jsName MouseEnterEvt = "onMouseEnter"
jsName MouseLeaveEvt = "onMouseLeave"


data EventHandler signal = EventHandler
    { handler :: RawEvent -> Maybe signal
    , evtType :: EvtType
    }

data ReactType
    = RtClass
    | RtBuiltin
    | RtSequence


-- | A 'ReactClass' is a standalone component of a user interface which
-- contains the state necessary to render itself. Classes are a tool for
-- scoping.
--
-- Use 'createClass' to construct.
data ReactClass props state sig = ReactClass
    { foreignClass :: IO JSAny
    }

    -- { classRender :: props -> state -> ReactElement RtBuiltin insig
    -- , classTransition :: insig -> state -> (state, sig)

    -- -- The IO action should occur only once
    -- , foreignClass :: IO ForeignClass

    -- -- TODO(joel) this conflicts weirdly with `className` from ReactElement.
    -- , className :: JSString
    -- , initialState :: state

    -- -- , stateRef :: IORef state
    -- -- , transitionRef :: IORef [sig]
    -- }



-- TODO use phantom type to indicate renderability? Only sequence is not.
data ReactNode sig
    = ComponentElement (ReactComponentElement sig)
    | DomElement (ReactDOMElement sig)
    | NodeText JSString
    | NodeSequence [ReactNode sig]


instance Monoid (ReactNode sig) where
    mempty = NodeSequence []

    (NodeSequence xs) `mappend` (NodeSequence ys) = NodeSequence (xs <> ys)
    (NodeSequence xs) `mappend` y = NodeSequence (xs <> [y])
    x `mappend` (NodeSequence ys) = NodeSequence (x : ys)
    x `mappend` y = NodeSequence [x, y]


reactNodeToJSAny :: (sig -> IO ()) -> ReactNode sig -> IO JSAny
reactNodeToJSAny sigHandler (ComponentElement elem) =
    componentToJSAny sigHandler elem
reactNodeToJSAny sigHandler (DomElement elem)       =
    domToJSAny sigHandler elem
reactNodeToJSAny sigHandler (NodeText str)          =
    castRef <$> toJSRef str
reactNodeToJSAny sigHandler (NodeSequence seq)      = do
    jsNodes <- mapM (reactNodeToJSAny sigHandler) seq
    castRef <$> toArray jsNodes


instance IsString (ReactNode sig) where
    fromString str = NodeText (fromString str)


attrsToJson :: [Attr] -> JSON
attrsToJson = Aeson.toJSON . H.fromList . map unAttr where
    unAttr (Attr name json) = (name, json)


separateAttrs :: [AttrOrHandler sig] -> ([Attr], [EventHandler sig])
separateAttrs attrHandlers = (map makeA as, map makeH hs) where
    (as, hs) = partition isAttr attrHandlers

    isAttr :: AttrOrHandler sig -> Bool
    isAttr (StaticAttr _ _) = True
    isAttr _ = False

    makeA :: AttrOrHandler sig -> Attr
    makeA (StaticAttr t j) = Attr t j

    makeH :: AttrOrHandler sig -> EventHandler sig
    makeH (Handler h) = h


attrHandlerToJSAny :: (sig -> IO ()) -> [AttrOrHandler sig] -> IO JSAny
attrHandlerToJSAny sigHandler attrHandlers = do
    let (attrs, handlers) = separateAttrs attrHandlers
    starter <- castRef <$> toJSRef (attrsToJson attrs)

    forM_ handlers $ makeHandler starter . unHandler sigHandler
    return starter


data AttrOrHandler signal
    = StaticAttr Text JSON
    | Handler (EventHandler signal)

data Attr = Attr Text JSON


data ReactComponentElement sig = ReactComponentElement
    { reComType :: IO JSAny
    , reComAttrs :: [AttrOrHandler sig]
    , reComChildren :: ReactNode sig
    , reComKey :: JSString
    , reComRef :: Maybe JSString
    }


componentToJSAny :: (sig -> IO ()) -> ReactComponentElement sig -> IO JSAny
componentToJSAny sigHandler (ReactComponentElement ty props children key ref) = do
    propsObj <- attrHandlerToJSAny sigHandler props

    keyProp <- toJSRef key
    setProp ("key" :: String) keyProp propsObj

    refProp <- toJSRef ref
    setProp ("ref" :: String) refProp propsObj

    ty' <- ty
    children' <- reactNodeToJSAny sigHandler children

    castRef <$> js_react_createElement_Class ty' propsObj children'


data ReactDOMElement sig = ReactDOMElement
    { reDomType :: JSString
    , reDomProps :: [AttrOrHandler sig]
    , reDomChildren :: ReactNode sig
    , reDomKey :: JSString
    , reDomRef :: Maybe JSString
    }


domToJSAny :: (sig -> IO ()) -> ReactDOMElement sig -> IO JSAny
domToJSAny sigHandler (ReactDOMElement ty props children key ref) = do
    propsObj <- attrHandlerToJSAny sigHandler props

    keyProp <- toJSRef key
    setProp ("key" :: String) keyProp propsObj

    refProp <- toJSRef ref
    setProp ("ref" :: String) refProp propsObj

    children' <- reactNodeToJSAny sigHandler children

    castRef <$> js_react_createElement_DOM ty propsObj children'


-- attributes

unHandler :: (s -> IO ())
          -> EventHandler s
          -> (RawEvent -> Maybe (IO ()), EvtType)
unHandler act (EventHandler handle ty) = (\e -> act <$> handle e, ty)


makeHandler :: JSAny
            -- ^ object to set this attribute on
            -> (RawEvent -> Maybe (IO ()), EvtType)
            -- ^ handler
            -> IO ()
makeHandler obj (handle, evtTy) = do
    handle' <- handlerToJs handle
    js_set_handler (jsName evtTy) handle' obj


-- | Make a javascript callback to synchronously execute the handler
handlerToJs :: (RawEvent -> Maybe (IO ())) -> IO (JSFun (RawEvent -> IO ()))
handlerToJs handle = syncCallback1 AlwaysRetain True $ \evt ->
    case handle evt of
        Nothing -> return ()
        Just x -> x


mkStaticAttr :: Aeson.ToJSON a => Text -> a -> AttrOrHandler sig
mkStaticAttr name = StaticAttr name . Aeson.toJSON


mkEventHandler :: (FromJSRef evt, NFData evt)
               => EvtType
               -> (evt -> Maybe signal)
               -> AttrOrHandler signal
mkEventHandler ty handle =
    -- XXX unsafe as fuck
    let handle' raw = case unsafePerformIO $ fromJSRef $ castRef raw of
            Just x -> trace "mkEventHandler just" $ handle x
            Nothing -> trace "mkEventHandler nothing" $ Nothing
    -- let handle' raw = handle $!! fromJust $ unsafePerformIO $ fromJSRef $ castRef raw
    in Handler (EventHandler handle' ty)


-- | Low level properties common to all events
data EventProperties e =
  EventProperties { bubbles :: !Bool
                  , cancelable :: !Bool
                  , currentTarget :: !e -- NativeElem
                  , defaultPrevented :: !Bool
                  , eventPhase :: !Int
                  , isTrusted :: !Bool
                    -- ,  nativeEvent :: DOMEvent
                    -- , preventDefault :: IO ()
                    -- ,  stopPropagation :: IO ()
                  , evtTarget :: !e -- NativeElem
                    --, timeStamp :: Date
                  , eventType :: !JSString -- type
                  }

instance NFData e => NFData (EventProperties e) where
    rnf (EventProperties a b c d e f g h) =
        a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` g `seq` h `seq` ()

data ModifierKeys =
  ModifierKeys { altKey :: !Bool
               , ctrlKey :: !Bool
               , metaKey :: !Bool
               , shiftKey :: !Bool
               } deriving (Eq, Show, Generic)

instance FromJSRef ModifierKeys where

instance NFData ModifierKeys where
    rnf (ModifierKeys a b c d) = a `seq` b `seq` c `seq` d `seq` ()

data MouseEvent =
  MouseEvent { -- mouseEventProperties :: !(EventProperties e)
               -- mouseModifierKeys :: !ModifierKeys
             -- , buttonNum :: !Int -- "button"
               -- , buttons :: Int
               clientX :: !Double
             , clientY :: !Double
             , pageX :: !Double
             , pageY :: !Double
               -- , relatedTarget :: Unpacked
             , screenX :: !Double
             , screenY :: !Double
             } deriving (Show, Generic)

instance FromJSRef MouseEvent where

instance NFData MouseEvent where
    -- rnf (MouseEvent a b c d e f g h) =
    --     a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` g `seq` h `seq` ()
    rnf (MouseEvent a b c d e f) =
        a `seq` b `seq` c `seq` d `seq` e `seq` f  `seq` ()

data KeyboardEvent =
  KeyboardEvent { -- keyboardEventProperties :: ! (EventProperties e)
                -- keyboardModifierKeys :: !ModifierKeys
                  charCode :: !Int
                , key :: !JSString
                , keyCode :: !Int
                -- , locale :: !JSString
                , location :: !Int
                , repeat :: !Bool
                , which :: !Int
                } deriving (Show, Generic)

instance FromJSRef KeyboardEvent where

instance NFData KeyboardEvent where
    rnf (KeyboardEvent a b c d e f) =
        a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` ()

data Target = Target
    { value :: !JSString
    , tagName :: !JSString
    -- XXX(joel) This is gross. Added a second field so that the generic
    -- FromJSRef instance does the right thing. Without a second field it
    -- uses the FromJSRef instance for `value`.
    } deriving (Show, Generic)

instance FromJSRef Target where

data ChangeEvent = ChangeEvent
    { target :: !Target
    , timeStamp :: !Int
    } deriving (Show, Generic)

instance FromJSRef ChangeEvent where

instance NFData ChangeEvent where
    rnf e@(ChangeEvent str stamp) = str `seq` stamp `seq` ()

data FocusEvent e =
  FocusEvent { -- focusEventProperties :: ! (EventProperties e)
               domEventTarget :: !e -- NativeElem
             , relatedTarget :: !e -- NativeElem
             }

instance NFData e => NFData (FocusEvent e) where
    rnf (FocusEvent a b) = a `seq` b `seq` ()
