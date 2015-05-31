{-# LANGUAGE NamedFieldPuns, OverloadedStrings, DataKinds #-}
module React.Class
    ( ReactClass(..)
    , createClass
    ) where


import Control.Monad
import Data.IORef

import GHCJS.Foreign
import GHCJS.Marshal

import React.Anim
import React.Imports
import React.Types


import GHCJS.Types


-- | A 'ReactClass' is a standalone component of a user interface which
-- contains the state necessary to render and animate itself. Classes are
-- a tool for scoping.
--
-- Use 'createClass' to construct.
data ReactClass state sig anim = ReactClass
    { classRender :: state -> React RtClass state sig anim ()
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
createClass :: (state -> React RtClass state sig anim ()) -- ^ render function
            -> (sig -> state -> (state, [AnimConfig sig anim]))
            -- ^ transition function
            -> state -- ^ initial state
            -> anim -- ^ initial animation state
            -> [sig] -- ^ signals to send on startup
            -> IO (ReactClass state sig anim)
createClass render transition initialState initialAnim initialTrans = do
    stateRef <- newIORef initialState
    animRef <- newIORef initialAnim
    runningAnimRef <- newIORef []
    transitionRef <- newIORef initialTrans

    -- renderCb <- syncCallback1 AlwaysRetain True (return . render <=< fromJSRef)
    renderCb <- syncCallback AlwaysRetain True $ do
        state <- readIORef stateRef
        return $ render state

    foreignClass <- js_createClass renderCb

    return $ ReactClass
        render
        transition
        foreignClass
        stateRef
        animRef
        runningAnimRef
        transitionRef
