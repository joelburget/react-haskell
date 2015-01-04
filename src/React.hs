-- |
-- Module      :  React
-- Copyright   :  (C) 2014-15 Joel Burget
-- License     :  MIT
-- Maintainer  :  Joel Burget <joelburget@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- React-Haskell is
--
module React
    ( module X

    -- React.Anim
    , Color(..)
    , getAnimationState
    , Animatable(..) -- XXX

    -- React.Class
    , ReactClass()
    , createClass

    -- React.Local
    , locally
    , Narrowing(..)

    -- React.Render
    , cancelRender
    , render

    -- React.Types
    , ReactT(..)
    , React
    , AnimationState
    , ClassState
    , Signal
    , RenderHandle(..)
    , AnimConfig(..)
    , Easing(..)
    , EventProperties(..)
    , ModifierKeys(..)
    , MouseEvent(..)
    , KeyboardEvent(..)
    , ChangeEvent(..)
    , FocusEvent(..)
    ) where

-- TODO
-- restricted monads
-- store elem in monad
-- escaping / dangerouslySetInnerHTML

import React.Anim
import React.Class
-- import React.Imports
-- import React.Interpret
import React.Local
import React.Render
import React.Types

import React.Attrs as X
import React.Elements as X
import React.Events as X
