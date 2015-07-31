-- |
-- Module      :  React
-- Copyright   :  (C) 2014-15 Joel Burget
-- License     :  MIT
-- Maintainer  :  Joel Burget <joelburget@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Usage:
--
-- This tutorial assumes only a basic understanding of React, the DOM, and
-- browser events. I recomment at least skimming the [official React
-- tutorial](https://facebook.github.io/react/docs/tutorial.html).
--
-- Let's start with a basic example:
--
-- @
-- page_ :: ReactNode Void
-- page_ =
--     let cls = smartClass
--             { name = "page"
--
--             -- initially the input is empty
--             , initialState = ""
--
--             -- always transition to the input's new value
--             , transition = \(_, value) -> (value, Nothing)
--
--             , renderFn = \_ str -> div_ [ class_ "container" ] $ do
--                 input_ [ value_ str, onChange (Just . value . target) ]
--             }
--     in classLeaf cls ()
--
-- main :: IO ()
-- main = do
--     Just doc <- currentDocument
--     Just elem <- documentGetElementById doc ("elem" :: JSString)
--     render page_ elem
-- @
--
-- In this example we defined a React class with 'Text' state, but taking only
-- @()@ as a prop. It's possible to use anything for props and state --
-- numbers, JSON, even React classes.
--
-- In the example the input always contains the state from the class, and the
-- state is updated on every input change event -- effectively, every
-- keystroke.
module React
    (
    -- * Classes
      ReactClass()
    , ClassConfig(..)
    , ClassCtx
    , smartClass
    , dumbClass

    -- * Rendering
    , render
    , debugRender

    -- * React Nodes
    , ReactNode

    -- * Events
    , module React.Events

    -- * Local
    , module React.Local

    -- XXX(joel)
    , AttrOrHandler()

    -- TODO - create React.Internal module for these?
    -- * Attributes
    , module React.Attrs

    -- * Creating Elements
    , module React.Elements

    -- * JS Interop
    , ImportedClass

    -- * PropTypes
    , PropRequired(IsRequired, IsntRequired)
    , PropType(..)
    , PropTypable
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
import React.PropTypes
