{-# LANGUAGE NamedFieldPuns #-}
module React.Anim.Class
    ( ReactClass(..)
    , createClass
    , ReactA'
    , ReactClassA'
    ) where

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
                , renderHandle :: Maybe RenderHandle
                }

type ReactA' state sig anim = React' (WithAnimState state sig anim) sig
type ReactClassA' state sig anim = ReactClass (WithAnimState state sig anim) sig

-- This class can wrap V.ReactClass, but only if the V.ReactClass transition lives inside the IO monad, or the render lives inside the IO monad.
-- Instead, it will just have to be a different implementation
createClass :: (state -> anim -> React (WithAnimState state sig anim) sig ())
            -> (sig -> state -> (state, [AnimConfig sig anim]))
            -> state
            -> anim
            -> [sig]
            -> IO (ReactClass (WithAnimState state sig anim) sig)
createClass render transition initialState anim initialSigs = do

    let initialStateM = (\this -> do

            time <- js_performance_now
            rh <- js_raf . toPtr $ animTick this transition

            let state = foldl
                          (flip $ wrapTrans transition time)
                          (WithAnimState
                            initialState
                            anim
                            []
                            $ Just rh)
                          initialSigs

            return state)



    foreignClass <- js_createClass
                        (toPtr $ classForeignRender render transition)
                        (toPtr initialStateM)

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

    let (WithAnimState ustate a ra rh) = fromPtr pstate

    runIdentity $
      interpret (classRender ustate a) (updateCb this classTransition)

updateCb :: ForeignClassInstance
         -> (sig -> state -> (state, [AnimConfig sig anim]))
         -> sig
         -> IO ()
updateCb this trans sig = do
    time <- js_performance_now
    state <- fromPtr =<< js_getState this

    let newState = wrapTrans trans time sig state

    case renderHandle newState of
      Just h -> js_cancelRaf h
      Nothing -> return ()

    newHandle <- js_raf . toPtr $ animTick this trans

    js_setState
      this
      $ toPtr
        newState{renderHandle=Just newHandle}


animTick :: ForeignClassInstance
         -> (sig -> state -> (state, [AnimConfig sig anim]))
         -> Double
         -> IO ()
animTick this trans time = do

    state@WithAnimState{runningAnims} <- fromPtr =<< js_getState this

    let (runningAnims', endingAnims) = partition
          (\(RunningAnim AnimConfig{duration} beganAt) ->
              beganAt + duration > time)
          runningAnims

        endAnimSigTimes = mapMaybe
            (\(RunningAnim AnimConfig{ duration
                                     , onComplete
                                     }
                           beganAt) -> do

                sig <- onComplete True
                return ( sig
                       , beganAt + duration
                       )
            )
           endingAnims

        newState@(WithAnimState _ anim newRunningAnims _) = foldl
                     (\st (sig, time) ->
                       wrapTrans trans time sig st)
                       state{
                         runningAnims=runningAnims'
                       }
                       endAnimSigTimes

        runningAnims'' = zip newRunningAnims $ map (lerp time) newRunningAnims
        newAnim = stepRunningAnims anim (runningAnims'')

    newHandle <- js_raf $ toPtr $ animTick this trans

    js_setState this $ toPtr $ newState{ anim=newAnim
                                       , renderHandle=Just newHandle
                                       }

wrapTrans :: (sig -> state -> (state, [AnimConfig sig anim]))
           -> Double
           -> sig
           -> WithAnimState state sig anim
           -> WithAnimState state sig anim
wrapTrans trans
           time
           sig
           (WithAnimState ustate
                          anim
                          runningAnims
                          rh)
           = WithAnimState newUState anim newRunningAnims rh
    where (newUState, animConfs) = trans sig ustate
          newRunningAnims = runningAnims <> (zipWith RunningAnim animConfs (Data.List.repeat time))
