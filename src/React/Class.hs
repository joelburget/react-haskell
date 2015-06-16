{-# LANGUAGE NamedFieldPuns, OverloadedStrings, DataKinds,
    ExistentialQuantification #-}
module React.Class
    ( ReactClass(..)
    , ClassConfig(..)
    , createClass
    , smartClass
    , dumbClass
    ) where


import Control.Monad
import Data.IORef

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import React.Imports
import React.Types


data ClassConfig props state insig exsig = ClassConfig
    { renderFn :: props -> state -> ReactNode insig
    , getInitialState :: state
    , name :: JSString
    , transition :: (state, insig) -> (state, exsig)
    , startupSignals :: [insig]
    }


dumbClass :: ClassConfig props () sig sig
dumbClass = ClassConfig
    { name = "Anonymous Stateless Class"
    , renderFn = \_ _ -> "give this class a `render`!"
    , getInitialState = ()
    , transition = id
    , startupSignals = []
    }


smartClass :: ClassConfig props state insig exsig
smartClass = ClassConfig
    { name = "Anonymous Stateful Class"
    , renderFn = \_ _ -> "give this class a `render`!"
    , getInitialState = error "must define `getInitialState`!"
    , transition = error "must define `transition`!"
    , startupSignals = []
    }


createClass :: ClassConfig props state insig exsig
            -> ReactClass props state exsig
createClass ClassConfig{renderFn,
                        getInitialState,
                        name,
                        transition,
                        startupSignals} =

    let foreignObj = do
            obj <- newObj
            renderCb <- syncCallback1 AlwaysRetain True $ \returnObj -> do
                let rendered = renderFn undefined undefined
                    handler sig = putStrLn "some action should happen!"
                ret <- reactNodeToJSAny handler rendered
                setProp ("value" :: JSString) ret returnObj
            setProp ("render" :: JSString) renderCb obj
            setProp ("displayName" :: JSString) name obj
            return obj
        foreignClass = js_createClass <$> foreignObj

    in ReactClass foreignClass
           -- renderFn
           -- transition
           -- foreignClass
           -- name
           -- getInitialState
