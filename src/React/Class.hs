{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module React.Class
    ( ReactClass(classTransition, classState, classRender)
    , createClass
    ) where

import React.Anim
import React.Imports
import React.Types

import Haste
import Haste.JSON
import Haste.Prim

data ReactClass state trans anim signal = ReactClass
    { runningAnims :: [RunningAnim trans]
    , classRender :: state -> React anim trans ()
    , classTransition :: state -> trans -> (state, Maybe (AnimConfig trans))
    , foreignClass :: ForeignClass
    , classState :: state
    }

createClass :: (state -> React anim trans ())
            -> (state -> trans -> (state, Maybe (AnimConfig trans)))
            -> state
            -> IO (ReactClass state trans anim signal)
createClass render transition initialState = do
    foreignClass <- js_createClass (toPtr render)
    return $ ReactClass [] render transition foreignClass initialState
