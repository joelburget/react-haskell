{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module React.Class
    ( ReactClass(..)
    , createClass
    ) where

import Data.IORef
import Data.List
import Data.Monoid
import Data.Maybe
import Data.Functor.Identity
import React.Interpret

import React.Anim
import React.Imports
import React.Types

import Haste
import Haste.JSON
import Haste.Prim


-- | A 'ReactClass' is a standalone component of a user interface which
-- contains the state necessary to render and animate itself. Classes are
-- a tool for scoping.
--
-- Use 'createClass' to construct.
data ReactClass state sig anim =
  ReactClass { foreignClass :: ForeignClass
             , classTransition :: (sig -> state -> (state, [AnimConfig sig anim]))
             }


-- | 'ReactClass' smart constructor.
createClass :: (state -> React state sig anim ()) -- ^ render function
            -> (sig -> state -> (state, [AnimConfig sig anim]))
            -- ^ transition function
            -> state -- ^ initial state
            -> anim -- ^ initial animation state
            -> [sig] -- signals to send on startup
            -> IO (ReactClass state sig anim)
createClass render transition initialState initialAnim initialTrans = do
    animRef <- newIORef initialAnim
    runningAnimRef <- newIORef []
    transitionRef <- newIORef initialTrans

    foreignClass <- js_createClass
                      (toPtr $ classForeignRender render transition)
                      (toPtr initialState)
                      (toPtr $
                        ReactClassInstance
                          animRef
                          runningAnimRef
                          transitionRef)

    return $ ReactClass foreignClass transition

classForeignRender :: (state -> React state sig anim ())
                   -> (sig -> state -> (state, [AnimConfig sig anim]))
                   -> ReactClassInstance sig anim
                   -> state
                   -> IO ForeignNode
classForeignRender classRender
                   classTransition
                   ReactClassInstance { animRef
                                      , runningAnimRef
                                      , transitionRef
                                      }
                   prevState = do

  transitions <- readIORef transitionRef
  runningAnims <- readIORef runningAnimRef
  prevAnim <- readIORef animRef

  let time = 0

  let (newState, newAnims) =
        mapAccumL (flip classTransition) prevState transitions
      newAnims' = concat newAnims
      newRunningAnims = map (`RunningAnim` time) newAnims'

      (runningAnims', endingAnims) = partition
          (\(RunningAnim AnimConfig{duration} beganAt) ->
              beganAt + duration > time)
          (runningAnims <> newRunningAnims)

      endingAnims' = zip endingAnims [1..]
      runningAnims'' = zip runningAnims' (map (lerp time) runningAnims')
      newAnim = stepRunningAnims prevAnim (endingAnims' ++ runningAnims'')

      -- TODO should this run before or after rendering?
      -- TODO expose a way to cancel / pass False in that case
      endAnimTrans = mapMaybe
          (\anim -> onComplete (config anim) True)
          endingAnims

  foreignNode <- runIdentity $
      interpret (classRender newState) newAnim (updateCb transitionRef)

  writeIORef animRef newAnim
  writeIORef runningAnimRef runningAnims'
  writeIORef transitionRef endAnimTrans

  return foreignNode


updateCb :: IORef [signal] -> signal -> IO ()
updateCb ref update = modifyIORef ref (update:)
