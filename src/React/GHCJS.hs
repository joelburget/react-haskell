{-# LANGUAGE CPP #-}

#ifndef __GHCJS__
{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
#endif

module React.GHCJS
    ( currentDocument
    , Document
    , Element
    , JSAny
    , documentGetElementById

    -- * GHCJS stubs
#ifdef __GHCJS__
    , module X
#else
    , ForeignRetention(..)
    , JSRef(..)
    , JSFun
    , JSArray
    , JSString
    , FromJSString(..)
    , ToJSString(..)
    , FromJSRef(..)
    , ToJSRef(..)
    , castRef
    , newObj
    , setProp
    , eqRef
    , toArray
    , setProp
    , syncCallback1
    , syncCallback2
#endif
    ) where

-- Export useful things from GHCJS, or mocks of them if we're running in GHC

import qualified Data.Aeson as Aeson
import Data.String
import Data.Text (Text)

#ifdef __GHCJS__

import GHCJS.Foreign as X
import GHCJS.Marshal as X
import GHCJS.Types as X
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Types (Document, Element)
import GHCJS.DOM.Document (documentGetElementById)

#else

data Document
data Element
data JSRef a = JSRef
data JSString_
type JSFun = JSRef
type JSArray = JSRef
type JSString = JSRef JSString_

class ToJSString a where
    toJSString :: a -> JSString

class FromJSString a where
    fromJSString :: JSString -> a

class FromJSRef a where
    fromJSRef :: JSRef a -> IO (Maybe a)

class ToJSRef a where
    toJSRef :: a -> IO (JSRef a)

instance FromJSRef Aeson.Value
instance FromJSRef Int
instance (FromJSRef a, FromJSRef b) => FromJSRef (a, b)
instance FromJSRef (JSRef ())
instance ToJSRef Int
instance ToJSRef Aeson.Value
instance ToJSRef a => ToJSRef (Maybe a)
instance ToJSRef (JSRef a)
instance FromJSString String
instance FromJSString Text
instance FromJSString JSString
instance ToJSString String
instance ToJSString Text
instance ToJSString JSString
instance IsString JSString

currentDocument :: IO (Maybe Document)
currentDocument = undefined

documentGetElementById ::
    -- (IsDocument self, ToJSString elementId) =>
    self -> elementId -> IO (Maybe Element)
documentGetElementById = undefined

castRef :: JSRef a -> JSRef b
castRef _ = JSRef

newObj :: IO (JSRef a)
newObj = undefined

data ForeignRetention
    = NeverRetain
    | AlwaysRetain
    | DomRetain (forall a. JSRef a)

eqRef :: JSRef a -> JSRef a -> Bool
eqRef = undefined

toArray :: [JSRef a] -> IO (JSArray a)
toArray = undefined

setProp :: ToJSString a => a -> JSRef b -> JSRef c -> IO ()
setProp = undefined

syncCallback1 :: ForeignRetention
              -> Bool
              -> (JSRef a -> IO b)
              -> IO (JSFun (JSRef a -> IO b))
syncCallback1 = undefined

syncCallback2 :: ForeignRetention
              -> Bool
              -> (JSRef a -> JSRef b -> IO c)
              -> IO (JSFun (JSRef a -> JSRef b -> IO c))
syncCallback2 = undefined

#endif

type JSAny = JSRef ()
