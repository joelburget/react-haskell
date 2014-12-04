{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, GADTs, Rank2Types #-}
module React.Types where

import Control.Applicative
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

data EvtType
    = ChangeEvt
    | KeyDownEvt
    | KeyPressEvt
    | KeyUpEvt
    | ClickEvt
    | DoubleClickEvt
    | MouseEnterEvt
    | MouseLeaveEvt

data StatefulEventHandler s = StatefulEventHandler
    { handler :: s -> RawEvent -> s
    , evtType :: EvtType
    }

newtype RawEvent = RawEvent JSAny deriving (Pack, Unpack)

type Attrs = [(JSString, JSON)]

data ReactNode s = Parent JSString Attrs [StatefulEventHandler s] [ReactNode s]
                 | Leaf JSString Attrs [StatefulEventHandler s]
                 -- | Pre Attrs Handlers [ReactNode] -- it'd be super cool to restrict this to a string somehow (restrict the underlying monad so it can only set attrs and string?)
                 | Text String -- TODO(joel) JSString?

type MockLens s a = Functor f => (a -> f a) -> s -> f s

mockGet :: MockLens s a -> s -> a
mockGet l = getConst . l Const

mockMod :: MockLens s a -> (a -> a) -> s -> s
mockMod l f = runIdentity . l (Identity . f)

mockSet :: MockLens s a -> a -> s -> s
mockSet l a = mockMod l (const a)

handlerConvert :: MockLens a b
               -> StatefulEventHandler b
               -> StatefulEventHandler a
handlerConvert l (StatefulEventHandler f ty) = StatefulEventHandler
    (\a raw -> mockSet l (f (mockGet l a) raw) a)
    ty

nodeConvert :: MockLens a b -> ReactNode b -> ReactNode a
nodeConvert l (Parent name attrs handlers children) =
    Parent name attrs (map (handlerConvert l) handlers)
        (map (nodeConvert l) children)
nodeConvert l (Leaf name attrs handlers) =
    Leaf name attrs (map (handlerConvert l) handlers)
nodeConvert l (Text str) = Text str

newtype StatefulReactT s m a = StatefulReactT
    { runStatefulReactT :: s -> m ([ReactNode s], s, a) }

type StatefulReact s = StatefulReactT s Identity
type PureReact = StatefulReact () ()

getState :: Monad m => StatefulReactT s m s
getState = StatefulReactT $ \s -> return ([], s, s)

instance (Monad m, Monoid a) => Monoid (StatefulReactT s m a) where
    mempty = StatefulReactT $ \s -> return ([], s, mempty)
    mappend f1 f2 = StatefulReactT $ \s -> do
        ~(c1, s1, a) <- runStatefulReactT f1 s
        ~(c2, s2, b) <- runStatefulReactT f2 s1
        return (c1 <> c2, s2, a <> b)

instance Monad m => Functor (StatefulReactT s m) where
    fmap = liftM

instance Monad m => Applicative (StatefulReactT s m) where
    pure = return
    (<*>) = ap

instance (Monad m, a ~ ()) => IsString (StatefulReactT s m a) where
    fromString str = StatefulReactT $ \s -> return ([Text str], s, ())

instance Monad m => Monad (StatefulReactT s m) where
    return a = StatefulReactT $ \s -> return ([], s, a)
    m >>= f = StatefulReactT $ \s -> do
        ~(c1, s1, a) <- runStatefulReactT m s
        ~(c2, s2, b) <- runStatefulReactT (f a) s1
        return (c1 <> c2, s2, b)

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

instance Monad m => Attributable (StatefulReactT s m a) (JSString, JSON) where
    react <! attr = StatefulReactT $ \s -> do
        ~(children, s', a) <- runStatefulReactT react s
        return (children <!> attr, s', a)

instance Monad m =>
         Attributable (StatefulReactT s m a) (StatefulEventHandler s) where
    react <! attr = StatefulReactT $ \s -> do
        ~(children, s', a) <- runStatefulReactT react s
        return (children <!< attr, s', a)

instance Attributable (StatefulReactT s m a) x =>
         Attributable (StatefulReactT s m a -> StatefulReactT s m a) x where
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

(<!<) :: [ReactNode s] -> StatefulEventHandler s -> [ReactNode s]
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

data ModifierKeys =
  ModifierKeys { altKey :: !Bool
               , ctrlKey :: !Bool
               , metaKey :: !Bool
               , shiftKey :: !Bool
               } deriving (Eq, Show)

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

newtype ChangeEvent = ChangeEvent { targetValue :: JSString }

data FocusEvent e =
  FocusEvent { -- focusEventProperties :: ! (EventProperties e)
               domEventTarget :: !e -- NativeElem
             , relatedTarget :: !e -- NativeElem
             }
