{-# LANGUAGE NamedFieldPuns #-}
module React.Anim.Class where

import Lens.Family2
import Data.Functor.Identity
import Data.Monoid
import Data.List
import Data.Maybe
import Haste.Prim

import React.Types
import React.Imports
import React.Interpret
import React.Anim

import qualified React.Class as V



-- Animation is now a kind of middleware between React JS and ReactClass.


data WithAnimState u sig anim =
  WithAnimState { userState :: u
                , anim :: anim
                , runningAnims :: [RunningAnim sig anim]
                }

-- This class can wrap V.ReactClass, but only if the V.ReactClass transition lives inside the IO monad, or the render lives inside the IO monad.
-- Instead, it will just have to be a different implementation
createClass :: (state -> anim -> React (WithAnimState state sig anim) sig ())
            -> (sig -> state -> (state, [AnimConfig sig anim]))
            -> state
            -> anim
            -> [sig]
            -> IO (ReactClass (WithAnimState state sig anim) sig)
createClass render transition initialState anim initialTrans = do

    foreignClass <- js_createClass
                        (toPtr $ classForeignRender render transition)
                        (toPtr $ WithAnimState
                          initialState
                          anim
                          [])

    return $ ReactClass foreignClass


classForeignRender :: (state -> anim -> React (WithAnimState state sig anim) sig ())
                   -> (sig -> state -> (state, [AnimConfig sig anim]))
                   -> ForeignClassInstance
                   -> Ptr (WithAnimState state sig anim)
                   -> IO ForeignNode
classForeignRender classRender
                   classTransition
                   this
                   pstate = do

    let (WithAnimState ustate a ra) = fromPtr pstate

    runIdentity $
      interpret (classRender ustate a) (updateCb this classTransition)

updateCb :: ForeignClassInstance
         -> (sig -> state -> (state, [AnimConfig sig anim]))
         -> sig
         -> IO ()
updateCb this trans sig = do
    time <- js_performance_now
    state@WithAnimState{userState, anim, runningAnims} <- fromPtr =<< js_getState this

    let (newState, newAnims) = trans sig userState

        newRunningAnims = map (`RunningAnim` time) newAnims

    js_raf $ toPtr $ animTick this
    js_setState this $ toPtr $ WithAnimState newState anim runningAnims


animTick :: ForeignClassInstance
         -> Double
         -> IO ()
animTick this time = do

    state@WithAnimState{userState, anim, runningAnims} <- fromPtr =<< js_getState this

    let (runningAnims', endingAnims) = partition
          (\(RunningAnim AnimConfig{duration} beganAt) ->
              beganAt + duration > time)
          runningAnims

        endingAnims' = zip endingAnims [1..]
        runningAnims'' = zip runningAnims' (map (lerp time) runningAnims')
        newAnim = stepRunningAnims anim (endingAnims' ++ runningAnims'')

        endAnimTrans = mapMaybe
            (\anim -> onComplete (config anim) True)
            endingAnims

    js_raf $ toPtr $ animTick this
    js_setState this $ toPtr $ WithAnimState userState newAnim (map fst runningAnims'')

