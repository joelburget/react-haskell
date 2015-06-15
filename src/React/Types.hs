{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, TypeFamilies,
    ExistentialQuantification, ImpredicativeTypes, LiberalTypeSynonyms,
    DeriveGeneric, DataKinds, GADTs, OverloadedStrings, CPP #-}

#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif

module React.Types where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Functor.Identity
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text
import GHC.Generics
import System.IO.Unsafe

import qualified Data.Aeson as Aeson
import Data.Void
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import Lens.Family2

import Debug.Trace

#ifdef __GHCJS__
foreign import javascript unsafe "React.createElement.apply(null, [$1, $2].concat($3))" js_react_createElement_DOM :: JSString -> JSAny -> JSAny -> IO JSAny
#else
js_react_createElement_DOM :: JSString -> JSAny -> JSAny -> IO JSAny
js_react_createElement_DOM = error "cannot evaluate js_react_createElement_DOM in ghc"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "React.createElement.apply(null, [$1, $2].concat($3))" js_react_createElement_Class :: JSAny -> JSAny -> JSAny -> IO JSAny
#else
js_react_createElement_Class :: JSAny -> JSAny -> JSAny -> IO JSAny
js_react_createElement_Class = error "cannot evaluate js_react_createElement_Class in ghc"
#endif

instance Show JSString where
    show = fromJSString

type JSAny = JSRef ()

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

data EventHandler signal = EventHandler
    { handler :: RawEvent -> Maybe signal
    , evtType :: EvtType
    }

-- newtype RawEvent = RawEvent JSAny
data RawEvent_
type RawEvent = JSRef RawEvent_

type Attrs = [(JSString, JSON)]

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


instance ToJSRef (ReactNode sig) where
    toJSRef (ComponentElement elem) = castRef <$> toJSRef elem
    toJSRef (DomElement elem) = castRef <$> toJSRef elem
    toJSRef (NodeText str) = castRef <$> toJSRef str
    toJSRef (NodeSequence seq) = castRef <$> toJSRefListOf seq


instance IsString (ReactNode sig) where
    fromString str = NodeText (fromString str)


data ReactComponentElement sig = ReactComponentElement
    { reComType :: IO JSAny
    , reComProps :: IO JSAny
    , reComChildren :: ReactNode sig
    , reComKey :: JSString
    , reComRef :: Maybe JSString
    }

instance ToJSRef (ReactComponentElement sig) where
    toJSRef (ReactComponentElement ty props children key ref) = do
        propsObj <- props

        keyProp <- toJSRef key
        setProp ("key" :: String) keyProp propsObj

        refProp <- toJSRef ref
        setProp ("ref" :: String) refProp propsObj

        ty' <- ty
        children' <- castRef <$> toJSRef children

        castRef <$> js_react_createElement_Class ty' propsObj children'

data ReactDOMElement sig = ReactDOMElement
    { reDomType :: JSString
    , reDomProps :: JSON
    , reDomChildren :: ReactNode sig
    , reDomKey :: JSString
    , reDomRef :: Maybe JSString
    }

instance ToJSRef (ReactDOMElement sig) where
    toJSRef (ReactDOMElement ty props children key ref) = do
        propsObj <- castRef <$> toJSRef props

        keyProp <- toJSRef key
        setProp ("key" :: String) keyProp propsObj

        refProp <- toJSRef ref
        setProp ("ref" :: String) refProp propsObj

        children' <- castRef <$> toJSRef children

        castRef <$> js_react_createElement_DOM ty propsObj children'


-- attributes

data AttrOrHandler signal
    = StaticAttr JSString JSON
    | Handler (EventHandler signal)

data Attr = Attr Text JSON


mkStaticAttr :: Aeson.ToJSON a => JSString -> a -> AttrOrHandler signal
mkStaticAttr name = StaticAttr name . Aeson.toJSON


mkEventHandler :: (FromJSRef signal, NFData signal)
               => EvtType
               -> (signal -> Maybe signal')
               -> AttrOrHandler signal'
mkEventHandler ty handle =
    -- XXX unsafe as fuck
    let handle' raw = case unsafePerformIO $ fromJSRef $ castRef raw of
            Just x -> trace "mkEventHandler just" $ handle x
            Nothing -> trace "mkEventHandler nothing" $ Nothing
    -- let handle' raw = handle $!! fromJust $ unsafePerformIO $ fromJSRef $ castRef raw
    in Handler (EventHandler handle' ty)


separateAttrs :: [AttrOrHandler signal] -> ([EventHandler signal], Attrs)
separateAttrs [] = ([], [])
separateAttrs (StaticAttr k v:xs) =
    let (hs, as) = separateAttrs xs in (hs, (k, v):as)
separateAttrs (Handler h:xs) =
    let (hs, as) = separateAttrs xs in (h:hs, as)


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
