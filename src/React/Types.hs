{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
    FlexibleContexts, TypeFamilies, ExistentialQuantification,
    ImpredicativeTypes, LiberalTypeSynonyms, DataKinds, GADTs,
    OverloadedStrings, CPP #-}

module React.Types where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import qualified Data.Aeson as Aeson
import Data.List (partition)
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)

import React.GHCJS
import React.Imports
import React.Registry


instance Show JSString where
    show = fromJSString

type JSON = Aeson.Value

data EvtType
   = ChangeEvt
   | BlurEvt
   | FocusEvt
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


-- | A 'ReactClass' is a standalone component of a user interface which
-- contains the state necessary to render itself. Classes are a tool for
-- scoping.
--
-- Use 'createClass' to construct.
--
-- * 'props': The type of props passed in.
-- * 'state': The type of state this class maintains.
-- * 'insig': The type of signals this class handles (see 'classTransition')
-- * 'exsig': The type of signals this class emits (see 'classTransition')
-- * 'ctx': This is only used for React's mythical context feature -- If you know what that is, see 'childContext' for usage.
data ReactClass props state insig exsig ctx = ReactClass
    { classForeign :: JSAny
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

    | forall props. ToJSRef props =>
        ForeignClass (ImportedClass props sig) props (ReactNode sig)

    | NodeText Text
    | NodeSequence [ReactNode sig]
    | forall insig. LocalNode (insig -> sig) (ReactNode insig)


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


data ReactComponentElement exsig = forall props state insig ctx.
    ReactComponentElement
    { reComType :: ReactClass props state insig exsig ctx
    , reComChildren :: ReactNode insig
    , reComKey :: Maybe JSString
    , reComRef :: Maybe JSString

    -- Props are stored here, not used until `render`, because we need both the
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
