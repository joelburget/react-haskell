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

import React.Anim
import React.Attrs
import React.Class
import React.Elements
import React.Events
import React.Imports
import React.Interpret
import React.Types


doRender :: Elem -> Double -> ReactClass state sig anim -> IO ()
doRender elem time ReactClass{ classRender,
                               classTransition,
                               transitionRef,
                               runningAnimRef,
                               animRef,
                               stateRef } = do

    transitions <- readIORef transitionRef
    runningAnims <- readIORef runningAnimRef
    prevState <- readIORef stateRef
    prevAnim <- readIORef animRef

    let (newState, newAnims) =
            mapAccumL (flip classTransition) prevState transitions

        newAnims' = concat newAnims
        newRunningAnims = map (`RunningAnim` time) newAnims'

        (runningAnims', endingAnims) = partition
            (\(RunningAnim AnimConfig{duration} beganAt) ->
                beganAt + duration > time)
            (runningAnims <> newRunningAnims)

        endingAnims' = zip endingAnims [1..]
        runningAnims'' = zip runningAnims' (map (lerp time) runningAnims')
        newAnim = stepRunningAnims prevAnim (endingAnims' ++ runningAnims'')

        -- TODO should this run before or after rendering?
        -- TODO expose a way to cancel / pass False in that case
        endAnimTrans = mapMaybe
            (\anim -> onComplete (config anim) True)
            endingAnims

    foreignNode <- runIdentity $
        interpret (classRender newState) newAnim (updateCb transitionRef)
    js_render foreignNode elem

    writeIORef stateRef newState
    writeIORef animRef newAnim
    writeIORef runningAnimRef runningAnims'
    writeIORef transitionRef endAnimTrans


updateCb :: IORef [signal] -> signal -> IO ()
updateCb ref update = modifyIORef ref (update:)


-- XXX don't think the handle remains valid. fix this with a ref.
render :: Elem
       -> ReactClass state sig anim
       -> IO RenderHandle
render elem cls@ReactClass{transitionRef, runningAnimRef} = do
    let renderCb :: JSRef Double -> IO ()
        renderCb timeRef = do
            Just time <- fromJSRef timeRef
            transitions <- readIORef transitionRef
            runningAnims <- readIORef runningAnimRef

            -- only rerender when dirty
            when (length transitions + length runningAnims > 0) $
                doRender elem time cls

            raf
            return ()

        raf :: IO RenderHandle
        raf = js_raf =<< syncCallback1 AlwaysRetain True renderCb

    doRender elem 0 cls
    raf


cancelRender :: RenderHandle -> IO ()
cancelRender = js_cancelRaf
