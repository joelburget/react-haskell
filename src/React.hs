
-- Module      :  React
-- Copyright   :  (C) 2014-15 Joel Burget
-- License     :  MIT
-- Maintainer  :  Joel Burget <joelburget@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
module React
    (
    -- * Classes
      ReactClass()
    , ClassConfig(..)
    , smartClass
    , dumbClass

    -- * Rendering
    , render
    , debugRender

    -- * React Nodes
    , ReactNode

    -- * Events
    , EventProperties(..)
    , Target(..)
    , ModifierKeys(..)
    , MouseEvent(..)
    , KeyboardEvent(..)
    , ChangeEvent(..)
    , FocusEvent(..)

    -- * Local
    , module React.Local

    -- XXX(joel)
    , AttrOrHandler()

    -- TODO - create React.Internal module for these?
    -- * Attributes
    , module React.Attrs

    -- * Rebindable Syntax
    , module React.Rebindable

    -- * Creating Elements
    , module React.Elements
    ) where

-- TODO
-- restricted monads
-- store elem in monad
-- escaping / dangerouslySetInnerHTML

import React.Class
import React.Imports
import React.Local
import React.Render
import React.Types

import React.Attrs
import React.Elements
import React.Events
import React.Rebindable
