{-# LANGUAGE OverloadedStrings, FlexibleContexts, NamedFieldPuns, GADTs #-}

module React.Render
    ( render
    , debugRender
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
import Data.Void

import React.Attrs
import React.Class
import React.Elements
import React.Events
import React.Imports
import React.Interpret
import React.Types


-- `Void` forces our top-level class `transition` to always choose `Nothing`
-- over outputting a signal.
render :: ReactNode Void -> Elem -> IO ()
render node elem = do
    -- XXX
    node' <- reactNodeToJSAny undefined 0 node
    js_render node' elem


debugRender :: Show sig => ReactNode sig -> Elem -> IO ()
debugRender node elem = do
    -- XXX
    node' <- reactNodeToJSAny print 0 node
    js_render node' elem
