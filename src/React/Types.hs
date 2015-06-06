{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, TypeFamilies,
    ExistentialQuantification, ImpredicativeTypes, LiberalTypeSynonyms,
    DeriveGeneric, DataKinds, GADTs #-}
{-# LANGUAGE JavaScriptFFI #-}
module React.Types where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Functor.Identity
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.String
import GHC.Generics
import System.IO.Unsafe

import Data.Void
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import Lens.Family2

import Debug.Trace

instance Show JSString where
    show = fromJSString

type JSAny = JSRef ()

-- XXX
data JSON
  = Num  Double
  | Str  JSString
  | Bool Bool
  | Arr  [JSON]
  | Dict [(JSString, JSON)]
  | Null

instance IsString JSON where
  fromString = Str . fromString

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

-- it'd be super cool to restrict `Pre` to a string somehow (restrict the
-- underlying monad so it can only set attrs and string?)

data ReactNode signal
    = Parent ForeignRender Attrs [EventHandler signal] [Child signal]
    | Leaf ForeignRender Attrs [EventHandler signal]
    -- | Pre Attrs Handlers [ReactNode]
    | Text String -- TODO(joel) JSString?


-- A child is either a sequence of static nodes or *one* dynamic node. This
-- representation is motivated by ...
--
-- React.createElement("div", null, a, [b, c], d)
--
-- Now React can infer that `a` and `d` are static, while `b` and `c` are
-- dynamic.
--
-- XXX

data Child sig
    = Static (ReactNode sig)
    | Dynamic [(Int, ReactNode sig)]


-- Idea:
--
-- React is the type representing
-- * classes
-- * builtins
-- * sequences
--
-- Use a phantom type to only allow rendering of first two.

data ReactType
    = RtClass
    | RtBuiltin
    | RtSequence

-- link to smart / dumb components
-- https://medium.com/@dan_abramov/smart-and-dumb-components-7ca2f9a7c7d0


-- | A 'ReactClass' is a standalone component of a user interface which
-- contains the state necessary to render itself. Classes are a tool for
-- scoping.
--
-- Use 'createClass' to construct.
data ReactClass state sig = ReactClass
    { classRender :: state -> React RtBuiltin sig
    , classTransition :: sig -> state -> state

    -- The IO action should occur only once
    , foreignClass :: IO ForeignClass

    , className :: JSString
    , initialState :: state

    -- , stateRef :: IORef state
    -- , transitionRef :: IORef [sig]
    }


-- phew, what a mouthful
data React :: ReactType -> * -> * where

    -- even this could maybe just store the class name and attrs?
    -- XXX store ForeignClass
    ReactTClass    :: ReactClass state sig -> React RtClass sig

    -- This could really store just the name and attrs
    ReactTBuiltin  :: [Child sig] -> React RtBuiltin sig

    ReactTSequence :: [Child sig] -> React RtSequence sig


runReactT :: React ty sig -> [Child sig]
-- XXX
runReactT (ReactTClass cls) = runReactT ((classRender cls) (initialState cls))
runReactT (ReactTBuiltin children) = children
runReactT (ReactTSequence children) = children


type Pure a = a () Void ()

instance IsString (React RtBuiltin sig) where
    fromString str = ReactTBuiltin [Static (Text str)]


-- attributes

data AttrOrHandler signal
    = StaticAttr JSString JSON
    | Handler (EventHandler signal)


mkStaticAttr :: JSString -> (a -> JSON) -> a -> AttrOrHandler signal
mkStaticAttr name f a = StaticAttr name (f a)


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
