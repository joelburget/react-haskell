{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module React.Class
    ( ReactClass(..)
    , createClass
    ) where

import Data.IORef

import React.Anim
import React.Imports
import React.Types

import Haste
import Haste.JSON
import Haste.Prim


-- | A 'ReactClass' is a standalone component of a user interface which
-- contains the state necessary to render and animate itself. Classes are
-- a tool for scoping.
--
-- Use 'createClass' to construct.
data ReactClass state sig anim = ReactClass
    { classRender :: state -> React state sig anim ()
    , classTransition :: sig
                      -> state
                      -> (state, [AnimConfig sig anim])

    , foreignClass :: ForeignClass

    , stateRef :: IORef state
    , animRef :: IORef anim
    , runningAnimRef :: IORef [RunningAnim sig anim]
    , transitionRef :: IORef [sig]
    }


-- | 'ReactClass' smart constructor.
createClass :: (state -> React state sig anim ()) -- ^ render function
            -> (sig -> state -> (state, [AnimConfig sig anim]))
            -- ^ transition function
            -> state -- ^ initial state
            -> anim -- ^ initial animation state
            -> [sig] -- signals to send on startup
            -> IO (ReactClass state sig anim)
createClass render transition initialState initialAnim initialTrans = do
    foreignClass <- js_createClass $ toPtr render

    stateRef <- newIORef initialState
    animRef <- newIORef initialAnim
    runningAnimRef <- newIORef []
    transitionRef <- newIORef initialTrans

    return $ ReactClass
        render
        transition
        foreignClass
        stateRef
        animRef
        runningAnimRef
        transitionRef
