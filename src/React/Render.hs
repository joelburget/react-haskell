{-# LANGUAGE OverloadedStrings, FlexibleContexts, NamedFieldPuns #-}

module React.Render
    ( render
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String

import Haste hiding (fromString)
import Haste.Foreign
import Haste.JSON
import Haste.Prim

import React.Anim
import React.Attrs
import React.Class
import React.Elements
import React.Events
import React.Imports
import React.Interpret
import React.Local
import React.Types





render :: ReactClass state sig anim
       -> Elem
       -> IO ()
render ReactClass{foreignClass, classTransition} elem = js_render foreignClass elem

