{-# LANGUAGE OverloadedStrings, FlexibleContexts, NamedFieldPuns, GADTs #-}

module React.Render
    ( render
    , cancelRender
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

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import React.Attrs
import React.Class
import React.Elements
import React.Events
import React.Imports
import React.Interpret
import React.Types


-- XXX don't think the handle remains valid. fix this with a ref.
render :: Elem
       -> React ty state sig
       -> IO ()
render elem (ReactTClass cls) =
    render' elem ((classRender cls) (initialState cls))
render elem description@(ReactTBuiltin _) = render' elem description
render elem description@(ReactTSequence _) = render' elem description


render' :: Elem -> React ty state sig -> IO ()
render' elem render = do
    let renderCb :: IO ()
        renderCb = do
            foreignNode <- interpret render (const (return ())) -- XXX
            js_render foreignNode elem
            -- raf
            return ()
        -- raf :: IO RenderHandle
        -- raf = js_raf =<< syncCallback AlwaysRetain True renderCb
    renderCb
    -- raf


updateCb :: IORef [signal] -> signal -> IO ()
updateCb ref update = modifyIORef ref (update:)


cancelRender :: RenderHandle -> IO ()
cancelRender = js_cancelRaf
