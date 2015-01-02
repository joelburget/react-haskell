module React
    ( module X

    -- React.Class
    , ReactClass()
    , createClass

    -- React.Anim
    , Easing(..)
    , Color(..)
    , interpolate
    , getAnimState
    , Animatable(..) -- XXX

    -- React.Render
    , cancelRender
    , render
    ) where

-- TODO
-- * restricted monads
-- * store elem in monad
-- * escaping / dangerouslySetInnerHTML

import React.Anim
import React.Attrs as X
import React.Class
import React.Elements as X
import React.Events as X
import React.Imports
import React.Interpret
import React.Local as X
import React.Render
import React.Types as X
