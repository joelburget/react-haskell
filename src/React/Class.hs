{-# LANGUAGE NamedFieldPuns, OverloadedStrings, DataKinds #-}
module React.Class
    ( ReactClass(..)
    , createClass
    ) where


import Control.Monad
import Data.IORef

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import React.Imports
import React.Types


-- | 'React RtClass' smart constructor.
createClass :: (state -> React RtBuiltin state sig) -- ^ render function
            -> (sig -> state -> state) -- ^ transition function
            -> state -- ^ initial state
            -> [sig] -- ^ signals to send on startup
            -> IO (React RtClass state sig)
createClass render transition initialState initialTrans = do
    stateRef <- newIORef initialState
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
        transitionRef
