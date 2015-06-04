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
            -> React RtClass state sig
createClass render transition initialState initialTrans =
    -- stateRef <- newIORef initialState
    -- transitionRef <- newIORef initialTrans

    -- -- renderCb <- syncCallback1 AlwaysRetain True (return . render <=< fromJSRef)

    -- XXX how to get object without going to IO?
    let foreignObj = do
            obj <- newObj
            renderCb <- syncCallback AlwaysRetain True $ do
                -- state <- readIORef stateRef
                return render
            setProp ("render" :: JSString) renderCb obj
            return obj
        foreignClass = js_createClass <$> foreignObj

    in ReactTClass $ ReactClass render transition foreignClass initialState
        -- stateRef
        -- transitionRef
