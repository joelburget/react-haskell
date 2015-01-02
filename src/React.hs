module React
    ( module X

    -- React.Anim
    , Easing(..)
    , Color(..)
    , getAnimationState
    , Animatable(..) -- XXX

    -- React.Class
    , ReactClass()
    , createClass

    -- React.Local
    , locally

    -- React.Render
    , cancelRender
    , render
    ) where

-- TODO
-- * restricted monads
-- * store elem in monad
-- * escaping / dangerouslySetInnerHTML

import React.Anim
import React.Class
-- import React.Imports
-- import React.Interpret
import React.Local
import React.Render

import React.Attrs as X
import React.Elements as X
import React.Events as X
import React.Types as X
