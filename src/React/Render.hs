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


-- | Render a top-level component.
--
-- Note that the rendered component can't possibly emit signals.
render :: ReactNode Void -> Element -> IO ()
render node elem = do
    node' <- reactNodeToJSAny undefined 0 node
    js_render node' elem


-- | Unlike 'render', 'debugRender' can render components that emit signals, as
-- long as they can be shown.
debugRender :: Show sig => ReactNode sig -> Element -> IO ()
debugRender node elem = do
    node' <- reactNodeToJSAny print 0 node
    js_render node' elem
