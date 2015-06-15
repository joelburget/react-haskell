{-# LANGUAGE OverloadedStrings #-}
module React.Attrs where

import Data.Text (Text)
import GHCJS.Types

import React.Types

key_ :: Text -> AttrOrHandler signal
key_ = mkStaticAttr "key"

class_ :: Text -> AttrOrHandler signal
class_ = mkStaticAttr "className"

href_ :: Text -> AttrOrHandler signal
href_ = mkStaticAttr "href"

id_ :: Text -> AttrOrHandler signal
id_ = mkStaticAttr "id"

src_ :: Text -> AttrOrHandler signal
src_ = mkStaticAttr "src"

style_ :: JSON -> AttrOrHandler signal
style_ = mkStaticAttr "style"

value_ :: Text -> AttrOrHandler signal
value_ = mkStaticAttr "value"

placeholder_ :: Text -> AttrOrHandler signal
placeholder_ = mkStaticAttr "placeholder"

for_ :: Text -> AttrOrHandler signal
for_ = mkStaticAttr "htmlFor"

type_ :: Text -> AttrOrHandler signal
type_ = mkStaticAttr "type"

checked_ :: Bool -> AttrOrHandler signal
checked_ = mkStaticAttr "checked"

autofocus_ :: Bool -> AttrOrHandler signal
autofocus_ = mkStaticAttr "autoFocus"

width_ :: Double -> AttrOrHandler signal
width_ = mkStaticAttr "width"

height_ :: Double -> AttrOrHandler signal
height_ = mkStaticAttr "height"

-- svg!

cx_ :: Double -> AttrOrHandler signal
cx_ = mkStaticAttr "cx"

cy_ :: Double -> AttrOrHandler signal
cy_ = mkStaticAttr "cy"

d_ :: Double -> AttrOrHandler signal
d_ = mkStaticAttr "d"

dx_ :: Double -> AttrOrHandler signal
dx_ = mkStaticAttr "dx"

dy_ :: Double -> AttrOrHandler signal
dy_ = mkStaticAttr "dy"

x_ :: Double -> AttrOrHandler signal
x_ = mkStaticAttr "x"

y_ :: Double -> AttrOrHandler signal
y_ = mkStaticAttr "y"

r_ :: Double -> AttrOrHandler signal
r_ = mkStaticAttr "r"

fill_ :: Text -> AttrOrHandler signal
fill_ = mkStaticAttr "fill"

viewBox_ :: Text -> AttrOrHandler signal
viewBox_ = mkStaticAttr "viewBox"

points_ :: Text -> AttrOrHandler signal
points_ = mkStaticAttr "points"

transform_ :: Text -> AttrOrHandler signal
transform_ = mkStaticAttr "transform"

-- fillOpacity fontFamily fontSize fx fy gradientTransform
-- gradientUnits markerEnd markerMid markerStart offset opacity
-- patternContentUnits patternUnits preserveAspectRatio r rx ry
-- spreadMethod stopColor stopOpacity stroke strokeDasharray strokeLinecap
-- strokeOpacity strokeWidth textAnchor version x1 x2 x y1 y2 y
