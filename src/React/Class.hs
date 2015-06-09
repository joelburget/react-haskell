{-# LANGUAGE NamedFieldPuns, OverloadedStrings, DataKinds #-}
module React.Class
    ( ReactClass(..)
    , ClassConfig(..)
    , createClass
    , statefulClass
    , statelessClass
    ) where


import Control.Monad
import Data.IORef

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import React.Imports
import React.Types


statelessClass :: ClassConfig props () sig
statelessClass = ClassConfig
    { name = "Anonymous Stateless Class"
    , renderFn = \_ _ -> "give this class a `render`!"
    , getInitialState = ()
    , transition = flip const
    , startupSignals = []
    }


statefulClass :: ClassConfig props state sig
statefulClass = ClassConfig
    { name = "Anonymous Stateful Class"
    , renderFn = \_ _ -> "give this class a `render`!"
    , getInitialState = error "must define `getInitialState`!"
    , transition = flip const
    , startupSignals = []
    }


-- type SmartComponent = ReactClass


-- | 'React RtClass' smart constructor.
createClass :: ClassConfig props state sig -> ReactClass props state sig
createClass ClassConfig{renderFn,
                        getInitialState,
                        name,
                        transition,
                        startupSignals} =

    let foreignObj = do
            obj <- newObj
            renderCb <- syncCallback AlwaysRetain True $ do
                -- state <- readIORef stateRef
                return renderFn
            setProp ("render" :: JSString) renderCb obj
            setProp ("displayName" :: JSString) name obj
            return obj
        foreignClass = js_createClass <$> foreignObj

    in ReactClass
           renderFn
           transition
           foreignClass
           name
           getInitialState
