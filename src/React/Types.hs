{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, TypeFamilies,
    ExistentialQuantification, ImpredicativeTypes, LiberalTypeSynonyms,
    DeriveGeneric #-}
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


keyed :: [(Int, ReactT state sig anim m a)] -> ReactT state sig anim m ()
keyed = mapM_ $ \(ix, m) -> ReactT $ \anim -> do
    (children, _) <- runReactT m anim
    -- ^ got a [Child sig], really need `ReactNode sig`
    return children


keyed :: [(Int, ReactT state sig anim m a)] -> ReactT state sig anim m a
keyed nodes = ReactT $ \anim -> do
    let f :: anim -> (Int, ReactT state sig anim m a) -> m
        f anim (key, m) = runReactT anim

    runReactT :: anim -> m ([Child sig], a)

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

data AnimConfig sig anim = forall a. (Animatable a) => AnimConfig {
      -- | How long this animation lasts in milliseconds
      duration :: Double
      -- | Where does this animation start and end?
    , endpoints :: (a, a)
      -- | Pointer to this field within 'AnimationState'
    , lens :: Lens' anim a
      -- | How is the animation eased?
    , easing :: Easing
      -- | Do something when it's finished?
    , onComplete :: Bool -> Maybe sig
    }


data RunningAnim sig anim = RunningAnim
    { config :: AnimConfig sig anim
    , beganAt :: Double
    }

-- Idea:
--
-- ReactT is the type representing
-- * classes
-- * builtins
-- * sequences
--
-- Use a phantom type to only allow rendering of first two.

newtype ReactT state sig anim m a = ReactT {
    runReactT :: anim -> m ([Child sig], a)
    }


-- newtype StaticReactT state sig anim m a =
--     StaticReactT { unStaticReactT :: ReactT state sig anim m a }
--     deriving (IsString, Monoid, Functor, Applicative, Monad)


-- newtype DynamicReactT state sig anim m a =
--     DynamicReactT { unDynamicReactT :: ReactT state sig anim m a }
--     deriving (IsString, Monoid, Functor, Applicative, Monad)


-- class GenericReactT result where
--     embed :: (anim -> m ([Child sig], a)) -> result state sig anim m a
--     unEmbed :: result state sig anim m a -> anim -> m ([Child sig], a)


-- -- instance GenericReactT ReactT where
-- --     embed = ReactT
-- --     unEmbed = runReactT


-- instance GenericReactT StaticReactT where
--     embed = StaticReactT . ReactT
--     unEmbed = runReactT . unStaticReactT


-- instance GenericReactT DynamicReactT where
--     embed = DynamicReactT . ReactT
--     unEmbed = runReactT . unDynamicReactT


-- -- Idea:
-- --
-- -- * You must use `dynamic_` to build a component with reorderable children.
-- --   - (by default we deal with dynamic values)
-- -- * Dynamic children must have a key. *
-- -- * Static children must not have a key. *
-- -- * Dynamic children are passed to createElement as an array.
-- -- * Static children are passed to createElement as parameters.
-- --
-- -- * I'm not sure it's possible to force the key thing.
-- dynamic_ :: ReactT state sig anim m a -> StaticReactT state sig anim m a
-- dynamic_ = StaticReactT


type Pure a = a () Void ()


type React state sig anim = ReactT state sig anim Identity
type React' state sig anim = ReactT state sig anim Identity ()

-- type StaticReact state sig anim = StaticReactT state sig anim Identity
-- type StaticReact' state sig anim = StaticReactT state sig anim Identity ()

-- type DynamicReact state sig anim = DynamicReactT state sig anim Identity
-- type DynamicReact' state sig anim = DynamicReactT state sig anim Identity ()


instance (Monad m, Monoid a) => Monoid (ReactT state sig anim m a) where
    mempty = ReactT $ \_ -> return ([], mempty)
    mappend f1 f2 = ReactT $ \anim -> do
        ~(c1, a) <- runReactT f1 anim
        ~(c2, b) <- runReactT f2 anim
        return (c1 <> c2, a <> b)


instance Monad m => Functor (ReactT state sig anim m) where
    fmap = liftM


instance Monad m => Applicative (ReactT state sig anim m) where
    pure = return
    (<*>) = ap


instance (Monad m, a ~ ()) => IsString (ReactT state sig anim m a) where
    fromString str = ReactT $ \_ -> return ([Static (Text str)], ())


instance Monad m => Monad (ReactT state sig anim m) where
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
