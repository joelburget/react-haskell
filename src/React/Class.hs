{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module React.Class
    ( ReactClass(..)
    , createClass
    ) where

import Data.List
import Data.Monoid
import Data.Maybe
import Data.Functor.Identity
import React.Interpret

import React.Imports
import React.Types

import Haste
import Haste.JSON
import Haste.Prim
--import Haste.Foreign


-- | A 'ReactClass' is a standalone component of a user interface which
-- contains the state necessary to render itself. Classes are
-- a tool for scoping.
--
-- Use 'createClass' to construct.
data ReactClass state sig =
  ReactClass { foreignClass :: ForeignClass
             , classTransition :: (sig -> state -> state)
             }


-- | 'ReactClass' smart constructor.
createClass :: (state -> React state sig ()) -- ^ render function
            -> (sig -> state -> state)
            -- ^ transition function
            -> state -- ^ initial state
            -> [sig] -- signals to send on startup
            -> IO (ReactClass state sig)
createClass render transition initialState initialTrans = do

    putStrLn "Creating Class"
    foreignClass <- js_createClass
                      (toPtr $ classForeignRender render transition)
                      (toPtr initialState)

    return $ ReactClass foreignClass transition

classForeignRender :: (state -> React state sig ())
                   -> (sig -> state -> state)
                   -> Ptr ForeignClassInstance
                   -> Ptr state
                   -> IO ForeignNode
classForeignRender classRender
                   classTransition
                   pthis
                   pstate = do

   putStrLn "classForeignRender start"
   n<-runIdentity $
        interpret (classRender $ fromPtr pstate) (updateCb pthis classTransition)
   putStrLn "classForeignRender done"
   return n

updateCb :: Ptr ForeignClassInstance -> (sig -> state -> state) -> sig -> IO ()
updateCb this trans sig = js_overState this $ toPtr (toPtr.(trans sig).fromPtr)

