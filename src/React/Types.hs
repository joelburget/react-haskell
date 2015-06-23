{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
    FlexibleContexts, TypeFamilies, ExistentialQuantification,
    ImpredicativeTypes, LiberalTypeSynonyms, DataKinds, GADTs,
    OverloadedStrings, CPP #-}

module React.Types where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.List (partition)
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)

import qualified Data.Aeson as Aeson

import React.Imports
import React.Registry


instance Show JSString where
    show = fromJSString

type JSON = Aeson.Value

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
    { handler :: Int -> RawEvent -> Maybe signal
    , evtType :: EvtType
    }

data ReactType
    = RtClass
    | RtBuiltin
    | RtSequence


-- | A 'ReactClass' is a standalone component of a user interface which
-- contains the state necessary to render itself. Classes are a tool for
-- scoping.
--
-- Use 'createClass' to construct.
data ReactClass props state insig exsig = ReactClass
    { classForeign :: JSAny
    , classRender :: props -> state -> ReactNode insig
    , classInitialState :: state
    , className :: JSString
    , classTransition :: (state, insig) -> (state, Maybe exsig)

    , classStateRegistry :: ClassRegistry props state insig exsig
    }


-- keyed :: [ (Int, ReactNode sig) ] -> ReactNode sig
-- keyed


-- assignKey :: ReactNode sig -> Int -> ReactNode sig
-- assignKey (ComponentElement (


-- TODO use phantom type to indicate renderability? Only sequence is not.
data ReactNode sig
    = ComponentElement (ReactComponentElement sig)
    | DomElement (ReactDOMElement sig)
    | NodeText JSString
    | NodeSequence [ReactNode sig]


instance Monoid (ReactNode sig) where
    mempty = NodeSequence []

    (NodeSequence xs) `mappend` (NodeSequence ys) = NodeSequence (xs <> ys)
    (NodeSequence xs) `mappend` y = NodeSequence (xs <> [y])
    x `mappend` (NodeSequence ys) = NodeSequence (x : ys)
    x `mappend` y = NodeSequence [x, y]


instance IsString (ReactNode sig) where
    fromString str = NodeText (fromString str)


data AttrOrHandler signal
    = StaticAttr Text JSON
    | Handler (EventHandler signal)


data ReactComponentElement exsig = forall props state insig. ReactComponentElement
    { reComType :: ReactClass props state insig exsig
    , reComAttrs :: [AttrOrHandler insig]
    , reComChildren :: ReactNode insig
    , reComKey :: Maybe JSString
    , reComRef :: Maybe JSString

    -- Props are stored here, not used until, `render` because we need both the
    -- props and state at the same time.
    , reComProps :: props

    -- We can't store the class id here because we don't know it until *after*
    -- render has run! It's not allocated until componentWillMount.
    -- , reComClassId :: Int
    }


data ReactDOMElement sig = ReactDOMElement
    { reDomType :: JSString
    , reDomProps :: [AttrOrHandler sig]
    , reDomChildren :: ReactNode sig
    , reDomKey :: Maybe JSString
    , reDomRef :: Maybe JSString
    }
