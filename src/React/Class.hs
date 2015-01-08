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
data ReactClass ty = ReactClass
    { classRender :: ClassState ty -> React ty ()
    , classTransition :: Signal ty
                      -> ClassState ty
                      -> (ClassState ty, [AnimConfig ty])

    , foreignClass :: ForeignClass

    , stateRef :: IORef (ClassState ty)
    , animRef :: IORef (AnimationState ty)
    , runningAnimRef :: IORef [RunningAnim ty]
    , transitionRef :: IORef [Signal ty]
    }


-- | 'ReactClass' smart contstructor.
createClass :: (ClassState ty -> React ty ()) -- ^ render function
            -> (Signal ty -> ClassState ty -> (ClassState ty, [AnimConfig ty]))
            -- ^ transition function
            -> ClassState ty -- ^ initial state
            -> AnimationState ty -- ^ initial animation state
            -> [Signal ty] -- signals to send on startup
            -> IO (ReactClass ty)
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
