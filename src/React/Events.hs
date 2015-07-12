{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, DeriveGeneric,
    FlexibleInstances #-}
module React.Events
    ( EventProperties(..)
    , Target(..)
    , ModifierKeys(..)
    , MouseEvent(..)
    , KeyboardEvent(..)
    , ChangeEvent(..)
    , FocusEvent(..)
    , BlurEvent(..)

    -- * Native Events
    , onBlur
    , onFocus
    , onChange
    , onKeyDown
    , onKeyPress
    , onKeyUp
    , onMouseEnter
    , onMouseLeave
    , onDoubleClick
    , onClick

    -- * Synthetic Events
    , onEnter
    ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Maybe
import GHC.Generics
import System.IO.Unsafe

import React.GHCJS
import React.Imports
import React.Types


mkEventHandler :: (FromJSRef evt, NFData evt)
               => EvtType
               -> (evt -> Maybe signal)
               -> AttrOrHandler signal
mkEventHandler ty handle =
    -- XXX unsafe as fuck
    -- XXX throwing away ix
    let handle' ix raw = case unsafePerformIO $ fromJSRef $ castRef raw of
            Just x -> handle $!! x
            Nothing -> Nothing
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

-- data FocusEvent e =
--   FocusEvent { -- focusEventProperties :: ! (EventProperties e)
--                domEventTarget :: !e -- NativeElem
--              , relatedTarget :: !e -- NativeElem
--              }

-- instance NFData e => NFData (FocusEvent e) where
--     rnf (FocusEvent a b) = a `seq` b `seq` ()

data FocusEvent = FocusEvent deriving Generic

instance NFData FocusEvent

instance FromJSRef FocusEvent where

data BlurEvent = BlurEvent deriving (Show, Generic)

instance FromJSRef BlurEvent where

instance NFData BlurEvent


-- XXX isn't this in GHCJS.Prim?
instance Eq JSString where
    (==) = eqRef

-- TODO: handle (a -> Maybe b) or (a -> b)

onBlur :: (BlurEvent -> Maybe s) -> AttrOrHandler s
onBlur = mkEventHandler BlurEvt

onFocus :: (FocusEvent -> Maybe s) -> AttrOrHandler s
onFocus = mkEventHandler FocusEvt

onChange :: (ChangeEvent -> Maybe s) -> AttrOrHandler s
onChange = mkEventHandler ChangeEvt

onKeyDown :: (KeyboardEvent -> Maybe s) -> AttrOrHandler s
onKeyDown = mkEventHandler KeyDownEvt

onKeyPress :: (KeyboardEvent -> Maybe s) -> AttrOrHandler s
onKeyPress = mkEventHandler KeyPressEvt

onKeyUp :: (KeyboardEvent -> Maybe s) -> AttrOrHandler s
onKeyUp = mkEventHandler KeyUpEvt

onMouseEnter :: (MouseEvent -> Maybe s) -> AttrOrHandler s
onMouseEnter = mkEventHandler MouseEnterEvt

onMouseLeave :: (MouseEvent -> Maybe s) -> AttrOrHandler s
onMouseLeave = mkEventHandler MouseLeaveEvt

onDoubleClick :: (MouseEvent -> Maybe s) -> AttrOrHandler s
onDoubleClick = mkEventHandler DoubleClickEvt

onClick :: (MouseEvent -> Maybe s) -> AttrOrHandler s
onClick = mkEventHandler ClickEvt

onEnter :: s -> AttrOrHandler s
onEnter s = onKeyPress handler where
    handler KeyboardEvent{key="Enter"} = Just s
    handler _ = Nothing
