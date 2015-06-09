
-- Module      :  React
-- Copyright   :  (C) 2014-15 Joel Burget
-- License     :  MIT
-- Maintainer  :  Joel Burget <joelburget@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
module React
    ( module X

    -- React.Class
    , ReactClass()
    , ClassConfig(..)
    , createClass
    , statelessClass
    , statefulClass

    -- React.Local
    , locally
    , GeneralizeSignal(..)

    -- React.Render
    , cancelRender
    , render

    -- React.Types
    , React(..)
    , ReactType(..)
    -- , React
    -- , React'
    , Pure
    , RenderHandle(..)
    , EventProperties(..)
    , Target(..)
    , ModifierKeys(..)
    , MouseEvent(..)
    , KeyboardEvent(..)
    , ChangeEvent(..)
    , FocusEvent(..)
    , ForeignRender

    -- XXX(joel)
    , JSON(..)

    -- XXX(joel)
    , AttrOrHandler()
    , RawAttrs(..)
    , ReactArray(..)
    , ForeignNode(..)
    , mkStaticAttr
    ) where

-- TODO
-- restricted monads
-- store elem in monad
-- escaping / dangerouslySetInnerHTML

import React.Class
-- import React.Imports
-- import React.Interpret
import React.Local
import React.Render
import React.Types

import React.Attrs as X
import React.Elements as X
import React.Events as X
import React.Rebindable as X
