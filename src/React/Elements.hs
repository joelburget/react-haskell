{-# LANGUAGE OverloadedStrings, FlexibleInstances, DataKinds,
    ConstraintKinds #-}
-- TODO(joel) rename to React.DOM?
module React.Elements
    ( domParent
    , domLeaf
    , classParent
    , classLeaf
    , foreignParent
    , foreignLeaf
    ) where

import Control.Applicative
import Data.Aeson as Aeson
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.Monoid
import Data.String
import System.IO.Unsafe

import React.Class
import React.GHCJS
import React.Imports
import React.Registry
import React.Types


-- | Parent nodes always take a list of arguments and children.
-- @
-- span_ [class_ "example"] $ ... children ...
-- @
--
-- TODO questionable whether foreign nodes should use ReactBuiltin. Maybe
-- create a ReactForeign?
-- TODO(joel) this is essentially createElement
domParent :: JSString
          -> [AttrOrHandler sig]
          -> ReactNode sig
          -> ReactNode sig
domParent name attrs children =
    DomElement (ReactDOMElement name attrs children Nothing Nothing)


domLeaf :: JSString
        -> [AttrOrHandler sig]
        -> ReactNode sig
domLeaf name attrs =
    DomElement (ReactDOMElement name attrs mempty Nothing Nothing)


classParent' :: ReactClass props state insig exsig
            -> [AttrOrHandler insig]
            -> ReactNode insig
            -> props
            -> ReactNode exsig
classParent' cls attrs children props = ComponentElement
    (ReactComponentElement cls attrs children Nothing Nothing props)


classLeaf' :: ReactClass props state insig exsig
          -> [AttrOrHandler insig]
          -> props
          -> ReactNode exsig
classLeaf' cls attrs props = ComponentElement
    (ReactComponentElement cls attrs mempty Nothing Nothing props)


classParent :: ClassConfig props state insig exsig
            -> [AttrOrHandler insig]
            -> ReactNode insig
            -> props
            -> ReactNode exsig
classParent = classParent' . createClass


classLeaf :: ClassConfig props state insig exsig
          -> [AttrOrHandler insig]
          -> props
          -> ReactNode exsig
classLeaf = classLeaf' . createClass


-- Note: state here reflects the state managed by react-haskell, not the state
-- of the component.
-- XXX how to get signal out!
foreignParent :: JSAny
              -> [AttrOrHandler sig]
              -> ReactNode sig
              -> props
              -> ReactNode sig
foreignParent obj = classParent' $ ReactClass
    obj
    (\((), sig) -> ((), Just sig))
    undefined


-- Note: state here reflects the state managed by react-haskell, not the state
-- of the component.
-- XXX how to get signal out!
foreignLeaf :: JSAny
            -> [AttrOrHandler sig]
            -> props
            -> ReactNode sig
foreignLeaf obj = classLeaf' $ ReactClass
    obj
    (\((), sig) -> ((), Just sig))
    undefined
