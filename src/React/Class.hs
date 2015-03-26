{-# LANGUAGE NamedFieldPuns, OverloadedStrings, BangPatterns, TypeFamilies,
  ConstraintKinds #-}
module React.Class
    ( ReactClass(..)
    , createClass
    ) where

import Control.Monad
import Data.IORef
import Data.List
import Data.Monoid
import Data.Maybe
import Data.Functor.Identity

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import React.Interpret
import React.Imports
import React.ElemTypes
import React.Types


toJSAny :: ToJSRef a => a -> IO JSAny
toJSAny = fmap castRef . toJSRef


-- | 'ReactClass' smart constructor.
createClass :: BiRef state
            => (state -> React state sig ()) -- ^ render function
            -> (sig -> state -> state)
            -- ^ transition function
            -> state -- ^ initial state
            -> [sig] -- ^ signals to send on startup
            -> IO (ReactClass state sig)
createClass render transition initialState initialTrans = do

    renderCb <- syncCallback2 AlwaysRetain True $ \inst st ->
        classForeignRender render transition inst st

    stateCb <- syncCallback1 AlwaysRetain True $ \_ -> return initialState

    foreignClassRef <- js_createClass renderCb stateCb
    Just foreignClass <- fromJSRef foreignClassRef

    return $ ReactClass foreignClass


classForeignRender :: BiRef state
                   => (state -> React state sig ())
                   -> (sig -> state -> state)
                   -> ForeignClassInstance
                   -> JSRef state
                   -> IO ForeignNode
classForeignRender classRender
                   classTransition
                   this
                   stateRef = do

    st <- fromJSRef stateRef
    runIdentity $
        interpret (classRender $ fromJust st) (updateCb this classTransition)


updateCb :: BiRef state
         => ForeignClassInstance
         -> (sig -> state -> state)
         -> sig
         -> IO ()
updateCb this trans sig = do
    cb <- syncCallback1 AlwaysRetain True $ \stateRef -> do
        Just state <- fromJSRef stateRef
        toJSRef $ trans sig state
    js_overState this cb
