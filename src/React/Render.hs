{-# LANGUAGE OverloadedStrings, FlexibleContexts, NamedFieldPuns #-}

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


doRender :: Elem -> Double -> ReactClass state sig -> IO ()
doRender elem time ReactClass{ classRender,
                               classTransition,
                               transitionRef,
                               stateRef } = do

    transitions <- readIORef transitionRef
    prevState <- readIORef stateRef

    let newState = foldl (flip classTransition) prevState transitions

    foreignNode <- interpret (classRender newState) (updateCb transitionRef)
    js_render foreignNode elem

    writeIORef stateRef newState
    writeIORef transitionRef []


updateCb :: IORef [signal] -> signal -> IO ()
updateCb ref update = modifyIORef ref (update:)


-- XXX don't think the handle remains valid. fix this with a ref.
render :: Elem
       -> ReactClass state sig
       -> IO RenderHandle
render elem cls@ReactClass{transitionRef} = do
    let renderCb :: JSRef Double -> IO ()
        renderCb timeRef = do
            Just time <- fromJSRef timeRef
            transitions <- readIORef transitionRef

            -- only rerender when dirty
            when (length transitions > 0) $
                doRender elem time cls

            raf
            return ()

        raf :: IO RenderHandle
        raf = js_raf =<< syncCallback1 AlwaysRetain True renderCb

    doRender elem 0 cls
    raf


cancelRender :: RenderHandle -> IO ()
cancelRender = js_cancelRaf
