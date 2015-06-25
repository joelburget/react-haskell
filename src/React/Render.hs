{-# LANGUAGE OverloadedStrings, FlexibleContexts, NamedFieldPuns, GADTs #-}

module React.Render
    ( render
    , debugRender
    ) where

import Data.Void

import React.GHCJS
import React.Imports
import React.Interpret
import React.Types


-- `Void` forces our top-level class `transition` to always choose `Nothing`
-- over outputting a signal.
render :: ReactNode Void -> Element -> IO ()
render node elem = do
    -- XXX
    node' <- reactNodeToJSAny undefined 0 node
    js_render node' elem


debugRender :: Show sig => ReactNode sig -> Element -> IO ()
debugRender node elem = do
    -- XXX
    node' <- reactNodeToJSAny print 0 node
    js_render node' elem
