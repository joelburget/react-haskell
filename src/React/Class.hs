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


data ReactClass state signal anim = ReactClass
    { classRender :: state -> React anim signal ()
    , classTransition :: state -> signal -> (state, [AnimConfig signal anim])

    , foreignClass :: ForeignClass

    , stateRef :: IORef state
    , animRef :: IORef [RunningAnim signal anim]
    , transitionRef :: IORef [signal]
    }


createClass :: (state -> React anim signal ())
            -> (state -> signal -> (state, [AnimConfig signal anim]))
            -> state
            -> [signal]
            -> IO (ReactClass state signal anim)
createClass render transition initialState initialTrans = do
    foreignClass <- js_createClass $ toPtr render

    stateRef <- newIORef initialState
    animRef <- newIORef []
    transitionRef <- newIORef initialTrans

    return $ ReactClass
        render
        transition
        foreignClass
        stateRef
        animRef
        transitionRef
