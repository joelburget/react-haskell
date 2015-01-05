{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, TypeFamilies,
    ExistentialQuantification, ImpredicativeTypes #-}
module React.Types where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Functor.Identity
import Data.Monoid
import Data.String

import Data.Void
import Haste
import Haste.Foreign
import Haste.JSON
import Haste.Prim
import Lens.Family2


newtype ForeignNode = ForeignNode JSAny deriving (Pack, Unpack)
newtype RawAttrs = RawAttrs JSAny  deriving (Pack, Unpack)
newtype ReactArray = ReactArray JSAny deriving (Pack, Unpack)
newtype ForeignClass = ForeignClass JSAny deriving (Pack, Unpack)

newtype RenderHandle = RenderHandle Int
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

data EventHandler signal = EventHandler
    { handler :: RawEvent -> Maybe signal
    , evtType :: EvtType
    }

newtype RawEvent = RawEvent JSAny deriving (Pack, Unpack)

type Attrs = [(JSString, JSON)]

-- it'd be super cool to restrict `Pre` to a string somehow (restrict the
-- underlying monad so it can only set attrs and string?)

data ReactNode signal
    = Parent JSString Attrs [EventHandler signal] [ReactNode signal]
    | Leaf JSString Attrs [EventHandler signal]
    -- | Pre Attrs Handlers [ReactNode]
    | Text String -- TODO(joel) JSString?


-- | Standard easing functions. These are used to 'interpolate' smoothly.
--
-- See <http://joelburget.com/react-haskell/ here> for visualizations.
data Easing
    = Linear

    | EaseInQuad
    | EaseOutQuad
    | EaseInOutQuad

    | EaseInCubic
    | EaseOutCubic
    | EaseInOutCubic

    | EaseInQuart
    | EaseOutQuart
    | EaseInOutQuart

    | EaseInQuint
    | EaseOutQuint
    | EaseInOutQuint

    | EaseInElastic
    | EaseOutElastic
    | EaseInOutElastic

    | EaseInBounce
    | EaseOutBounce
    | EaseInOutBounce

    | EaseBezier Double Double Double Double
    | EaseInSine
    | EaseOutSine
    deriving (Show, Eq, Ord)

-- | Properties that can animate.
--
-- Numeric values like 'width' and 'height', as well as colors.
class Animatable a where
    -- TODO is `to` always `animZero`?
    -- | Use an easing function to interpolate between two values
    interpolate :: Easing -- ^ easing function
                -> a -- ^ from
                -> a -- ^ to
                -> Double -- ^ [0..1] ratio of /time/ elapsed
                -> a

    -- | Add two animations
    animAdd :: a -> a -> a

    -- | Subtract two animations
    animSub :: a -> a -> a
    animZero :: a


-- | A 'ReactKey' is a type, which conventionally has no constructors,
-- mapping to the type of state, animation state, and signals associated
-- with a page fragment or class.
--
-- Example:
--
-- @
-- data Slider -- note the key has no constructors
-- data SliderState = Open | Closed
-- data SliderSignal = SlideOpen | SlideClosed
--
-- instance ReactKey Slider where
--     type ClassState Slider = SliderState
--     type AnimationState Slider = Double
--     type Signal Slider = SliderSignal
--
-- -- this page fragment has access to the animation state 'Double' and can
-- -- emit 'SliderSignal's.
-- pageFragment :: React Slider ()
-- pageFragment = div_ ...
--
-- -- this class stores the class state and animation state. its internals
-- -- can emit `SliderSignal`s.
-- sliderClass :: ReactClass Slider ()
-- sliderClass = ...
-- @
class ReactKey ty where
    -- | The state needed to render a class (ignoring animation)
    type ClassState ty :: *

    -- | The state needed to animate a class
    type AnimationState ty :: *

    -- | The type of signals a class can send
    type Signal ty :: *


-- Unit's ClassState and AnimationState are uninteresting. Its Signal is
-- entirely uninhabited.
instance ReactKey () where
    type ClassState     () = ()
    type AnimationState () = ()
    type Signal         () = Void


-- things you might want to control about an animation:
-- * duration
-- * from
-- * to
-- * lens
-- * easing
-- * oncomplete
-- * chaining
-- * delay

-- possible configurations:
-- * set new state, animate from old to new at same time
--   - need to connect ClassState and AnimationState somehow
-- * animate manually from -> to

data AnimConfig ty = forall a. (Animatable a) => AnimConfig {
      -- | How long this animation lasts in milliseconds
      duration :: Double
      -- | Where does this animation start and end?
    , endpoints :: (a, a)
    -- , lens :: Lens' anim a XXX
      -- | Pointer to this field within 'AnimationState'
    , lens :: Traversal' (AnimationState ty) a
      -- | How is the animation eased?
    , easing :: Easing
      -- | Do something when it's finished?
    , onComplete :: Bool -> Maybe (Signal ty)
    }


data RunningAnim ty = RunningAnim
    { config :: AnimConfig ty
    , beganAt :: Double
    }


newtype ReactT ty m a = ReactT
    { runReactT :: AnimationState ty -> m ([ReactNode (Signal ty)], a) }


type React ty = ReactT ty Identity


instance (Monad m, Monoid a) => Monoid (ReactT ty m a) where
    mempty = ReactT $ \_ -> return ([], mempty)
    mappend f1 f2 = ReactT $ \anim -> do
        ~(c1, a) <- runReactT f1 anim
        ~(c2, b) <- runReactT f2 anim
        return (c1 <> c2, a <> b)


instance Monad m => Functor (ReactT ty m) where
    fmap = liftM


instance Monad m => Applicative (ReactT ty m) where
    pure = return
    (<*>) = ap


instance (Monad m, a ~ ()) => IsString (ReactT ty m a) where
    fromString str = ReactT $ \_ -> return ([Text str], ())


instance Monad m => Monad (ReactT ty m) where
    return a = ReactT $ \_ -> return ([], a)
    m >>= f = ReactT $ \anim -> do
        ~(c1, a) <- runReactT m anim
        ~(c2, b) <- runReactT (f a) anim
        return (c1 <> c2, b)


-- attributes

data AttrOrHandler signal
    = StaticAttr JSString JSON
    | Handler (EventHandler signal)


mkStaticAttr :: JSString -> (a -> JSON) -> a -> AttrOrHandler signal
mkStaticAttr name f a = StaticAttr name (f a)


mkEventHandler :: (NFData signal)
               => (RawEvent -> signal)
               -> EvtType
               -> (signal -> Maybe signal')
               -> AttrOrHandler signal'
mkEventHandler unNative ty handle =
    -- important - you must deepseq the event immediately - otherwise
    -- react's pooling will collect and destroy it.
    let handle' raw = handle $!! unNative raw
    in Handler (EventHandler handle' ty)


separateAttrs :: [AttrOrHandler signal] -> ([EventHandler signal], Attrs)
separateAttrs [] = ([], [])
separateAttrs (StaticAttr k v:xs) =
    let (hs, as) = separateAttrs xs in (hs, (k, v):as)
separateAttrs (Handler h:xs) =
    let (hs, as) = separateAttrs xs in (h:hs, as)


-- terms

-- | Parent nodes always take children, but can also optionally take a list
-- of arguments.
--
-- Example of the first case, which exercises the simpler instance:
--
-- @
-- div_ $ ... children ...
-- @
--
-- Example of the second, which exercises the more complicated instance:
--
-- @
-- span_ [class_ "example"] $ ... children ...
-- @
class TermParent result where
    -- | The argument to a parent term is either:
    --
    -- * a list of attributes (@[AttrOrHandler (Signal ty)]@), which leads
    --   to a result type of @ReactT ty m a -> ReactT ty m a@.
    --
    -- * or children (@ReactT ty m a@), which leads to a result type of
    --   @ReactT ty m a@.
    type TermParentArg result :: *

    termParent :: JSString -> TermParentArg result -> result


instance (Monad m, f ~ ReactT ty m a) => TermParent (f -> ReactT ty m a) where
    type TermParentArg (f -> ReactT ty m a) = [AttrOrHandler (Signal ty)]

    termParent name attrs children = ReactT $ \anim -> do
        ~(childNodes, a) <- runReactT children anim
        let (hs, as) = separateAttrs attrs
        return ([Parent name as hs childNodes], a)


instance Monad m => TermParent (ReactT ty m a) where
    type TermParentArg (ReactT ty m a) = ReactT ty m a

    termParent name children = ReactT $ \anim -> do
        ~(childNodes, a) <- runReactT children anim
        return ([Parent name [] [] childNodes], a)


termLeaf :: (Monad m, sig ~ Signal ty)
         => JSString
         -> [AttrOrHandler sig]
         -> ReactT ty m ()
termLeaf name attrs = ReactT $ \_ -> do
    let (hs, as) = separateAttrs attrs
    return ([Leaf name as hs], ())


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

newtype ChangeEvent = ChangeEvent { targetValue :: JSString } deriving Show

instance NFData ChangeEvent where
    rnf e@(ChangeEvent str) = str `seq` ()

data FocusEvent e =
  FocusEvent { -- focusEventProperties :: ! (EventProperties e)
               domEventTarget :: !e -- NativeElem
             , relatedTarget :: !e -- NativeElem
             }

instance NFData e => NFData (FocusEvent e) where
    rnf (FocusEvent a b) = a `seq` b `seq` ()
