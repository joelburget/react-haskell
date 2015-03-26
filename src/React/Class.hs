{-# LANGUAGE NamedFieldPuns, OverloadedStrings, BangPatterns, TypeFamilies #-}
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


-- | A 'ReactClass' is a standalone component of a user interface which
-- contains the state necessary to render itself. Classes are
-- a tool for scoping.
--
-- Use 'createClass' to construct.


-- | 'ReactClass' smart constructor.
createClass :: (state -> React state sig ()) -- ^ render function
            -> (sig -> state -> state)
            -- ^ transition function
            -> state -- ^ initial state
            -> [sig] -- ^ signals to send on startup
            -> IO (ReactClass state sig)
createClass render transition initialState initialTrans = do

    foreignClass <- js_createClass
                      (toPtr $ classForeignRender render transition)
                      (toPtr (\_ -> return initialState))

    return $ ReactClass foreignClass

classForeignRender :: (state -> React state sig ())
                   -> (sig -> state -> state)
                   -> ForeignClassInstance
                   -> Ptr state
                   -> IO ForeignNode
classForeignRender classRender
                   classTransition
                   this
                   pstate = do

    runIdentity $
        interpret (classRender $ fromPtr pstate) (updateCb this classTransition)

updateCb :: ForeignClassInstance
         -> (sig -> state -> state)
         -> sig
         -> IO ()
updateCb this trans sig = js_overState this $ toPtr (toPtr.(trans sig).fromPtr)
