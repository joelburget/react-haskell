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
#ifdef __GHCJS__
    , module X
#else
    , JSRef(..)
    , JSFun
    , JSArray
    , JSString
    , FromJSString(..)
    , ToJSString(..)
    , FromJSRef(..)
    , ToJSRef(..)
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

#endif

type JSAny = JSRef ()
