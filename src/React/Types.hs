{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, TypeFamilies, ExistentialQuantification, ImpredicativeTypes #-}
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

-- | The state needed to render a class (ignoring animation)
--
-- Example:
--
-- @
-- data instance ClassState Slider = Open | Closed
-- @
data family ClassState ty :: *

-- | The state needed to animate a class
--
-- Example:
--
-- @
-- data instance ClassState Slider = SlidingProgress Double
-- @
data family AnimationState ty :: *

-- | The type of signals a class can send
--
-- Example:
--
-- @
-- data instance Signal Slider = SlideOpen | SlideClosed
-- @
data family Signal ty :: *

data instance ClassState      () = UnitClassState
data instance AnimationState () = UnitAnimationState
data instance Signal         () = UnitSignal

data AnimConfig ty = forall a. (Animatable a, Show a) => AnimConfig {
      -- | How long this animation lasts in milliseconds
      duration :: Double
      -- | Where does this animation start (it goes to 'animZero')
    , from :: a
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

instance Monad m => Attributable (ReactT ty m a) (JSString, JSON) where
    react <! attr = ReactT $ \anim -> do
        ~(children, a) <- runReactT react anim
        return (children <!> attr, a)

instance (Monad m, sig ~ Signal ty) =>
         Attributable (ReactT ty m a) (EventHandler sig) where
    react <! attr = ReactT $ \anim -> do
        ~(children, a) <- runReactT react anim
        return (children <!< attr, a)

instance Attributable (ReactT ty m a) x =>
         Attributable (ReactT ty m a -> ReactT ty m a) x where
    f <! attr = (<! attr) . f

class Attributable h a where
    (<!) :: h -> a -> h

(<!?) :: Attributable h a => h -> (Bool, a) -> h
h <!? (True, a) = h <! a
h <!? (False, _) = h

(<!>) :: [ReactNode signal] -> (JSString, JSON) -> [ReactNode signal]
[elem] <!> attr = [go elem] where
    go (Parent name as hs cs) = Parent name (attr:as) hs cs
    go (Leaf name as hs)      = Leaf name (attr:as) hs
    go (Text str)             = Text str
_ <!> _ = error "attr applied to multiple elems!"

(<!<) :: [ReactNode signal] -> EventHandler signal -> [ReactNode signal]
[elem] <!< hndl = [go elem] where
    go (Parent name as hs cs) = Parent name as (hndl:hs) cs
    go (Leaf name as hs)      = Leaf name as (hndl:hs)
    go (Text str)             = Text str
_ <!< _ = error "handler applied to multiple elems!"

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
