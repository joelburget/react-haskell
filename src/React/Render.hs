{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module React.Render
    ( cancelRaf
    , render
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

cancelRaf :: RafHandle -> IO ()
cancelRaf = ffi "(function(id) { window.cancelAnimationFrame(id); })"

render :: -- (Functor anim, Monoid (anim Double))
       Elem
       -> ReactClass state trans anim signal
       -> state
       -> IO RafHandle
render elem cls state = do
    stateRef <- newIORef state
    animRef <- newIORef []
    transitionRef <- newIORef []

    let updateCb update = modifyIORef transitionRef (update:)

        doRender time transitions runningAnims = do
            prevState <- readIORef stateRef

            let (newState, newMaybeAnims) =
                    mapAccumL (classTransition cls) prevState transitions

                newAnims = catMaybes newMaybeAnims
                newRunningAnims = map (\conf -> RunningAnim conf time 0) newAnims

                (runningAnims', endingAnims) = partition
                    (\(RunningAnim (AnimConfig duration _ _) beganAt progress) ->
                        beganAt + duration > time)
                    (runningAnims <> newRunningAnims)

                runningAnims'' = fmap (lerp' time) runningAnims'

                -- runningAnims''' = foldr animAdd animZero runningAnims''

                cls' = cls{classState=newState}

            -- TODO should this run before or after rendering?
            -- TODO expose a way to cancel / pass False in that case
            let endAnimTrans = mapMaybe
                    (\anim -> onComplete (config anim) True)
                    endingAnims

            foreignNode <- runIdentity $
                interpret (classRender cls' newState) runningAnims'' updateCb
            render' elem foreignNode

            writeIORef stateRef newState
            writeIORef animRef runningAnims'
            writeIORef transitionRef []


        renderCb time = do
            transitions <- readIORef transitionRef
            runningAnims <- readIORef animRef

            -- only rerender when dirty
            when (length transitions + length runningAnims > 0)
                (doRender time transitions runningAnims)

            js_raf $ toPtr renderCb
            return ()

    doRender 0 [] []
    js_raf $ toPtr renderCb
