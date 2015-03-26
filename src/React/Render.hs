{-# LANGUAGE NamedFieldPuns #-}
module React.Render where

import React.Types
import React.Imports


render :: Elem -> ReactClass state sig -> IO ()
render elem ReactClass{foreignClass} = js_render foreignClass elem
