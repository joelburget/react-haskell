{-# LANGUAGE GeneralizedNewtypeDeriving, ForeignFunctionInterface,
    MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module React.Types where

import Control.Applicative
import Data.Monoid
import Data.String

import Haste
import Haste.Foreign
import Haste.JSON
import Haste.Prim

-- TODO(joel) - move to imports?
foreign import ccall "js_id" jsText :: JSString -> ForeignNode

newtype ForeignNode = ForeignNode JSAny deriving (Pack, Unpack)
newtype RawAttrs = RawAttrs JSAny  deriving (Pack, Unpack)
newtype ReactArray = ReactArray JSAny deriving (Pack, Unpack)
newtype EventHandler = EventHandler {unEventHandler :: RawAttrs -> IO ()}

instance IsString ForeignNode where
    fromString = jsText . toJSStr

newtype RawMouseEvent = RawMouseEvent JSAny deriving (Pack, Unpack)
newtype RawChangeEvent = RawChangeEvent JSAny deriving (Pack, Unpack)
newtype RawKeyboardEvent = RawKeyboardEvent JSAny deriving (Pack, Unpack)
newtype RawFocusEvent = RawFocusEvent JSAny deriving (Pack, Unpack)

type Attrs = [(JSString, JSON)]
type Handlers = [EventHandler]

data ReactNode = Parent JSString Attrs Handlers [ReactNode]
               | Leaf JSString Attrs Handlers
               -- | Pre Attrs Handlers [ReactNode] -- it'd be super cool to restrict this to a string somehow (restrict the underlying monad so it can only set attrs and string?)
               | Text String -- TODO(joel) JSString?

{-
instance Show ReactNode where
    show (Div as _ children) = "(Div " ++ show as ++ " " ++ show children ++ ")"
    show (Input as _) = "(Input " ++ show as ++ ")"
    show (Pre as _ children) = "(Pre " ++ show as ++ " " ++ show children ++ ")"
    show (Text str) = str
-}

data ReactM a = ReactM
    { attrs :: Attrs
    , handlers :: Handlers
    , children :: [ReactNode]
    , other :: a
    }

type React = ReactM ()

instance Functor ReactM where
    f `fmap` react@ReactM{other=a} = react{other=f a}

instance Applicative ReactM where
    pure = ReactM [] [] []
    (ReactM af hf nf f) <*> (ReactM aa ha na a) =
        ReactM (af <> aa) (hf <> ha) (nf <> na) (f a)

instance Monad ReactM where
    return = pure
    (ReactM aa ha na a) >>= nf =
        let ReactM as hs ns a' = nf a
        in ReactM (aa <> as) (ha <> hs) (na <> ns) a'

instance IsString (ReactM a) where
    fromString str = ReactM [] [] [Text str] (error "this shouldn't be accessed")

class Attributable h a where
    (<!) :: h -> a -> h

(<!?) :: Attributable h a => h -> (Bool, a) -> h
h <!? (True, a) = h <! a
h <!? (False, _) = h

(<!>) :: [ReactNode] -> (JSString, JSON) -> [ReactNode]
[elem] <!> attr = [go elem] where
    go (Parent name as hs cs)  = Parent name (attr:as) hs cs
    go (Leaf name as hs)   = Leaf name (attr:as) hs
    go (Text str)      = Text str
_ <!> _ = error "attr applied to multiple elems!"

(<!<) :: [ReactNode] -> EventHandler -> [ReactNode]
[elem] <!< hndl = [go elem] where
    go (Parent name as hs cs)  = Parent name as (hndl:hs) cs
    go (Leaf name as hs)   = Leaf name as (hndl:hs)
    go (Text str)      = Text str

instance Attributable (ReactM b) (JSString, JSON) where
    (ReactM as hs ns x) <! attr = ReactM as hs (ns <!> attr) x

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
instance Attributable (ReactM b) EventHandler where
    (ReactM as hs ns x) <! hndl = ReactM as hs (ns <!< hndl) x

instance Attributable (ReactM c) a =>
         Attributable (ReactM b -> ReactM c) a where
    f <! attr = (<! attr) . f

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
