{-# LANGUAGE NamedFieldPuns #-}
module React.Render where

import React.Types
import React.Imports

import Haste.DOM


render :: ReactClass state sig -> Elem -> IO ()
render ReactClass{foreignClass} elem = js_render foreignClass elem
