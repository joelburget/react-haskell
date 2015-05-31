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


-- | 'ReactClass' smart constructor.
createClass :: (state -> React RtBuiltin state sig anim ()) -- ^ render function
            -> (sig -> state -> (state, [AnimConfig sig anim]))
            -- ^ transition function
            -> state -- ^ initial state
            -> anim -- ^ initial animation state
            -> [sig] -- ^ signals to send on startup
            -> IO (React RtClass state sig anim ())
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

    return $ ReactTClass $ ReactClass
        render
        transition
        foreignClass
        stateRef
        animRef
        runningAnimRef
        transitionRef
