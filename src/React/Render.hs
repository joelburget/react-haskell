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


render :: Elem
       -> ReactClass state sig anim
       -> IO RenderHandle
render elem cls@ReactClass{transitionRef, runningAnimRef} = do
    let renderCb time = do
            transitions <- readIORef transitionRef
            runningAnims <- readIORef runningAnimRef

            -- only rerender when dirty
            when (length transitions + length runningAnims > 0) $
                doRender elem time cls

            js_raf $ toPtr renderCb
            return ()

    doRender elem 0 cls
    js_raf $ toPtr renderCb


cancelRender :: RenderHandle -> IO ()
cancelRender = js_cancelRaf
