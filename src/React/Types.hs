{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, TypeFamilies,
    ExistentialQuantification, ImpredicativeTypes, LiberalTypeSynonyms,
    DeriveGeneric, ConstraintKinds #-}
{-# LANGUAGE JavaScriptFFI #-}
module React.Types where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Functor.Identity
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

import Data.IORef

type BiRef a = (FromJSRef a, ToJSRef a)

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


-- | A 'ReactClass' is a standalone component of a user interface which
-- contains the state necessary to render itself. Classes are
-- a tool for scoping.
--
-- Use 'createClass' to construct.
data ReactClass state sig =
  ReactClass { foreignClass :: ForeignClass
             }

type ForeignClassInstance = JSAny
newtype ForeignNode = ForeignNode JSAny
newtype RawAttrs = RawAttrs JSAny
newtype ReactArray = ReactArray JSAny
newtype ForeignClass = ForeignClass JSAny deriving FromJSRef
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
                                                            --
data ReactNode signal
    = Parent ForeignRender Attrs [EventHandler signal] [ReactNode signal]
    | Leaf ForeignRender Attrs [EventHandler signal]
    -- | Pre Attrs Handlers [ReactNode]
    | Text String -- TODO(joel) JSString?

newtype ReactT state sig m a = ReactT
    { runReactT :: m ([ReactNode sig], a) }


type React state sig = ReactT state sig Identity
type React' state sig = ReactT state sig Identity ()
type Pure a = a () Void ()


instance (Monad m, Monoid a) => Monoid (ReactT state sig m a) where
    mempty = ReactT $ return ([], mempty)
    mappend f1 f2 = ReactT $ do
        ~(c1, a) <- runReactT f1
        ~(c2, b) <- runReactT f2
        return (c1 <> c2, a <> b)


instance Monad m => Functor (ReactT state sig m) where
    fmap = liftM


instance Monad m => Applicative (ReactT state sig m) where
    pure = return
    (<*>) = ap


instance (Monad m, a ~ ()) => IsString (ReactT state sig m a) where
    fromString str = ReactT $ return ([Text str], ())


instance Monad m => Monad (ReactT state sig m) where
    return a = ReactT $ return ([], a)
    m >>= f = ReactT $ do
        ~(c1, a) <- runReactT m
        ~(c2, b) <- runReactT (f a)
        return (c1 <> c2, b)


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
    -- important - you must deepseq the event immediately - otherwise
    -- react's pooling will collect and destroy it.
    -- XXX unsafe as fuck
    let handle' raw = case unsafePerformIO $ fromJSRef $ castRef raw of
            Just x -> handle $!! x
            Nothing -> Nothing
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
                  keyboardModifierKeys :: !ModifierKeys
                , charCode :: !Int
                , key :: !JSString
                , keyCode :: !Int
                , locale :: !JSString
                , location :: !Int
                , repeat :: !Bool
                , which :: !Int
                } deriving (Show, Generic)

instance FromJSRef KeyboardEvent where

instance NFData KeyboardEvent where
    rnf (KeyboardEvent a b c d e f g h) =
        a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` g `seq` h `seq` ()

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

-- Useful for defining elements
class TermParent result where
    -- | The argument to a parent term is either:
    --
    -- * a list of attributes (@[AttrOrHandler (Signal ty)]@), which leads
    --   to a result type of @ReactT ty m a -> ReactT ty m a@.
    --
    -- * or children (@ReactT ty m a@), which leads to a result type of
    --   @ReactT ty m a@.
    type TermParentArg result :: *

    termParent :: ForeignRender -> TermParentArg result -> result


instance (Monad m, f ~ ReactT state sig m a) =>
        TermParent (f -> ReactT state sig m a) where
    type TermParentArg (f -> ReactT state sig m a) = [AttrOrHandler sig]

    termParent render attrs children = ReactT $ do
        ~(childNodes, a) <- runReactT children
        let (hs, as) = separateAttrs attrs
        return ([Parent render as hs childNodes], a)


instance Monad m => TermParent (ReactT state sig m a) where
    type TermParentArg (ReactT state sig m a) = ReactT state sig m a

    termParent render children = ReactT $ do
        ~(childNodes, a) <- runReactT children
        return ([Parent render [] [] childNodes], a)

