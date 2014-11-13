{-# LANGUAGE GeneralizedNewtypeDeriving, CPP, ForeignFunctionInterface #-}
module React.Types where

import Data.String

#ifdef __HASTE__
import Haste
import Haste.Foreign
import Haste.JSON
import Haste.Prim

foreign import ccall "js_id" jsText :: JSString -> React
#else
data Unpacked
data JSString = JSSTring deriving Show
data JSAny
data Ptr a
data Elem

data JSON
   = Num Double
   | Str JSString
   | Bool Bool
   | Arr [JSON]
   | Dict [(JSString, JSON)]
   | Null

instance IsString JSON where
    fromString = undefined

class Pack a where
    pack :: Unpacked -> a
class Unpack a where
    unpack :: a -> Unpacked

instance Pack JSAny where
    pack = undefined
instance Pack () where
    pack = undefined
instance Unpack JSAny where
    unpack = undefined
instance Unpack Elem where
    unpack = undefined
instance IsString JSString where
    fromString = undefined

jsText :: JSString -> React
jsText = undefined

toJSStr :: String -> JSString
toJSStr = undefined

fromJSStr :: JSString -> String
fromJSStr = undefined

toPtr :: a -> Ptr a
toPtr = undefined
fromPtr :: Ptr a -> a
fromPtr = undefined

class FFI a
instance Pack a => FFI (IO a)
instance (Unpack a, FFI b) => FFI (a -> b)

ffi :: FFI a => JSString -> a
ffi = undefined

withElem :: String -> (Elem -> m a) -> m a
withElem = undefined
#endif

newtype React = React JSAny deriving (Pack, Unpack)
newtype RawAttrs = RawAttrs JSAny  deriving (Pack, Unpack)
newtype ReactArray = ReactArray JSAny deriving (Pack, Unpack)
newtype EventHandler = EventHandler {unEventHandler :: RawAttrs -> IO ()}

instance IsString React where
    fromString = jsText . toJSStr

newtype RawMouseEvent = RawMouseEvent JSAny deriving (Pack, Unpack)
newtype RawChangeEvent = RawChangeEvent JSAny deriving (Pack, Unpack)
newtype RawKeyboardEvent = RawKeyboardEvent JSAny deriving (Pack, Unpack)
newtype RawFocusEvent = RawFocusEvent JSAny deriving (Pack, Unpack)
