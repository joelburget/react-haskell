{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, GADTs, Rank2Types #-}
module React.Types where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Functor.Identity
import Data.Monoid
import Data.String

import Haste
import Haste.Foreign
import Haste.JSON
import Haste.Prim

newtype ForeignNode = ForeignNode JSAny deriving (Pack, Unpack)
newtype RawAttrs = RawAttrs JSAny  deriving (Pack, Unpack)
newtype ReactArray = ReactArray JSAny deriving (Pack, Unpack)

newtype RafHandle = RafHandle Int
    deriving (Pack, Unpack)

data EvtType
    = ChangeEvt
    | KeyDownEvt
    | KeyPressEvt
    | KeyUpEvt
    | ClickEvt
    | DoubleClickEvt
    | MouseEnterEvt
    | MouseLeaveEvt

data EventHandler s = EventHandler
    { handler :: RawEvent -> Maybe s
    , evtType :: EvtType
    }

handlerConvert :: (local -> general)
               -> EventHandler local
               -> EventHandler general
handlerConvert f (EventHandler handle ty) =
    EventHandler (\raw -> f <$> handle raw) ty

handlerConvert' :: EventHandler () -> EventHandler general
handlerConvert' (EventHandler handle ty) = EventHandler (const Nothing) ty

newtype RawEvent = RawEvent JSAny deriving (Pack, Unpack)

type Attrs = [(JSString, JSON)]

-- it'd be super cool to restrict `Pre` to a string somehow (restrict the
-- underlying monad so it can only set attrs and string?)

data ReactNode s = Parent JSString Attrs [EventHandler s] [ReactNode s]
                 | Leaf JSString Attrs [EventHandler s]
                 -- | Pre Attrs Handlers [ReactNode]
                 | Text String -- TODO(joel) JSString?

nodeConvert1 :: (local -> general) -> ReactNode local -> ReactNode general
nodeConvert1 f (Parent name attrs handlers children) =
    Parent name attrs (map (handlerConvert f) handlers)
        (map (nodeConvert1 f) children)
nodeConvert1 f (Leaf name attrs handlers) =
    Leaf name attrs (map (handlerConvert f) handlers)
nodeConvert1 f (Text str) = Text str

nodeConvert2 :: ReactNode () -> ReactNode general
nodeConvert2 (Parent name attrs handlers children) =
    Parent name attrs (map handlerConvert' handlers)
        (map nodeConvert2 children)
nodeConvert2 (Leaf name attrs handlers) =
    Leaf name attrs (map handlerConvert' handlers)
nodeConvert2 (Text str) = Text str

newtype ReactT s m a = ReactT
    { runReactT :: m ([ReactNode s], a) }

type React s = ReactT s Identity
type PureReact = React () ()

instance (Monad m, Monoid a) => Monoid (ReactT s m a) where
    mempty = ReactT $ return ([], mempty)
    mappend f1 f2 = ReactT $ do
        ~(c1, a) <- runReactT f1
        ~(c2, b) <- runReactT f2
        return (c1 <> c2, a <> b)

instance Monad m => Functor (ReactT s m) where
    fmap = liftM

instance Monad m => Applicative (ReactT s m) where
    pure = return
    (<*>) = ap

instance (Monad m, a ~ ()) => IsString (ReactT s m a) where
    fromString str = ReactT $ return ([Text str], ())

instance Monad m => Monad (ReactT s m) where
    return a = ReactT $ return ([], a)
    m >>= f = ReactT $ do
        ~(c1, a) <- runReactT m
        ~(c2, b) <- runReactT (f a)
        return (c1 <> c2, b)

-- TODO thinking there should be some notion of single / multiple?
-- We should only ever apply an attribute / handler to one element here.
--
-- div <! attr $ ...
--
-- vs
--
-- (div >> div) <! attr
--
-- in fact, I think we should only ever apply attrs to
-- `React -> React`
--
-- except things with no children?
--
-- input <! attr

instance Monad m => Attributable (ReactT s m a) (JSString, JSON) where
    react <! attr = ReactT $ do
        ~(children, a) <- runReactT react
        return (children <!> attr, a)

instance Monad m =>
         Attributable (ReactT s m a) (EventHandler s) where
    react <! attr = ReactT $ do
        ~(children, a) <- runReactT react
        return (children <!< attr, a)

instance Attributable (ReactT s m a) x =>
         Attributable (ReactT s m a -> ReactT s m a) x where
    f <! attr = (<! attr) . f

class Attributable h a where
    (<!) :: h -> a -> h

(<!?) :: Attributable h a => h -> (Bool, a) -> h
h <!? (True, a) = h <! a
h <!? (False, _) = h

(<!>) :: [ReactNode s] -> (JSString, JSON) -> [ReactNode s]
[elem] <!> attr = [go elem] where
    go (Parent name as hs cs) = Parent name (attr:as) hs cs
    go (Leaf name as hs)      = Leaf name (attr:as) hs
    go (Text str)             = Text str
_ <!> _ = error "attr applied to multiple elems!"

(<!<) :: [ReactNode s] -> EventHandler s -> [ReactNode s]
[elem] <!< hndl = [go elem] where
    go (Parent name as hs cs) = Parent name as (hndl:hs) cs
    go (Leaf name as hs)      = Leaf name as (hndl:hs)
    go (Text str)             = Text str
_ <!< _ = error "handler applied to multiple elems!"

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
               } deriving (Eq, Show)

instance NFData ModifierKeys where
    rnf (ModifierKeys a b c d) = a `seq` b `seq` c `seq` d `seq` ()

data MouseEvent =
  MouseEvent { -- mouseEventProperties :: !(EventProperties e)
               mouseModifierKeys :: !ModifierKeys
             , buttonNum :: !Int -- "button"
               -- , buttons :: Int
             , clientX :: !Double
             , clientY :: !Double
             , pageX :: !Double
             , pageY :: !Double
               -- , relatedTarget :: Unpacked
             , screenX :: !Double
             , screenY :: !Double
             } deriving Show

instance NFData MouseEvent where
    rnf (MouseEvent a b c d e f g h) =
        a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` g `seq` h `seq` ()

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
                } deriving Show

instance NFData KeyboardEvent where
    rnf (KeyboardEvent a b c d e f g h) =
        a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` g `seq` h `seq` ()

newtype ChangeEvent = ChangeEvent { targetValue :: JSString }

instance NFData ChangeEvent where
    rnf e@(ChangeEvent str) = str `seq` ()

data FocusEvent e =
  FocusEvent { -- focusEventProperties :: ! (EventProperties e)
               domEventTarget :: !e -- NativeElem
             , relatedTarget :: !e -- NativeElem
             }

instance NFData e => NFData (FocusEvent e) where
    rnf (FocusEvent a b) = a `seq` b `seq` ()
