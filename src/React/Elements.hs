{-# LANGUAGE OverloadedStrings, FlexibleInstances, DataKinds,
    ConstraintKinds #-}
-- TODO(joel) rename to React.DOM?
module React.Elements
    (
    -- * Class creation
      classParent
    , classLeaf

    -- * JS Interop
    , exportClassLeaf
    -- , exportNode
    , importLeafClass
    , importParentClass

    , ExportedNode
    , ExportedClass
    ) where

import Control.Applicative
import Data.Aeson as Aeson
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.Monoid
import Data.String
import Data.Void
import System.IO.Unsafe

import React.Class
import React.GHCJS
import React.Imports
import React.Interpret
import React.Registry
import React.Types


classParent' :: ReactClass props state insig exsig ctx
             -> ReactNode insig
             -> props
             -> ReactNode exsig
classParent' cls children props = ComponentElement
    (ReactComponentElement cls children Nothing Nothing props)


classLeaf' :: ReactClass props state insig exsig ctx
           -> props
           -> ReactNode exsig
classLeaf' cls props = ComponentElement
    (ReactComponentElement cls mempty Nothing Nothing props)


classParent :: ClassCtx ctx
            => ClassConfig props state insig exsig ctx
            -> ReactNode insig
            -> props
            -> ReactNode exsig
classParent = classParent' . createClass


-- TODO refine this type to reflect that it can send back signals
classLeaf :: ClassCtx ctx
          => ClassConfig props state insig exsig ctx
          -> props
          -> ReactNode exsig
classLeaf = classLeaf' . createClass


data ExportedClass_
type ExportedClass = JSRef ExportedClass_

newtype ExportedNode sig = ExportedNode (IO JSAny)

instance ToJSRef (ExportedNode sig) where
    toJSRef (ExportedNode n) = castRef <$> n


-- XXX
-- Must make sure to get componentId to this class! Currently it's in props,
-- but maybe should move...
exportClassLeaf :: ClassCtx ctx
                => ClassConfig props state insig exsig ctx
                -> ExportedClass
exportClassLeaf conf =
    castRef $ classForeign $ createClass conf


-- XXX
-- Must make sure to update state in JS when it changes in HS!
-- exportNode :: (sig -> IO ()) -> ReactNode sig -> ExportedNode sig
-- exportNode handler = ExportedNode . reactNodeToJSAny handler undefined



-- Note: state here reflects the state managed by react-haskell, not the state
-- of the component.
-- TODO - figure out how to handle callbacks (change Void -> a)


importLeafClass :: ToJSRef props
                => ImportedClass props sig
                -> props
                -> ReactNode sig
importLeafClass elem props = ForeignClass elem props mempty



importParentClass :: ToJSRef props
                  => ImportedClass props sig
                  -> props
                  -> ReactNode sig
                  -> ReactNode sig
importParentClass elem props children = ForeignClass elem props children
