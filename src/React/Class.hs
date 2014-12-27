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


data ReactClass ty = ReactClass
    { classRender :: PageState ty -> React ty ()
    , classTransition :: PageState ty -> Signal ty -> (PageState ty, [AnimConfig ty])

    , foreignClass :: ForeignClass

    , stateRef :: IORef (PageState ty)
    , animRef :: IORef (AnimationState ty)
    , runningAnimRef :: IORef [RunningAnim ty]
    , transitionRef :: IORef [Signal ty]
    }


createClass :: (PageState ty -> React ty ())
            -> (PageState ty -> Signal ty -> (PageState ty, [AnimConfig ty]))
            -> PageState ty
            -> AnimationState ty
            -> [Signal ty]
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
