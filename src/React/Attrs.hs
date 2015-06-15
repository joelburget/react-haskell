{-# LANGUAGE OverloadedStrings #-}
module React.Attrs where

import Data.Text (Text)
import GHCJS.Types

import React.Types

key_ :: Text -> Attr
key_ = mkStaticAttr "key"

class_ :: Text -> Attr
class_ = mkStaticAttr "className"

href_ :: Text -> Attr
href_ = mkStaticAttr "href"

id_ :: Text -> Attr
id_ = mkStaticAttr "id"

src_ :: Text -> Attr
src_ = mkStaticAttr "src"

style_ :: JSON -> Attr
style_ = mkStaticAttr "style"

value_ :: Text -> Attr
value_ = mkStaticAttr "value"

placeholder_ :: Text -> Attr
placeholder_ = mkStaticAttr "placeholder"

for_ :: Text -> Attr
for_ = mkStaticAttr "htmlFor"

type_ :: Text -> Attr
type_ = mkStaticAttr "type"

checked_ :: Bool -> Attr
checked_ = mkStaticAttr "checked"

autofocus_ :: Bool -> Attr
autofocus_ = mkStaticAttr "autoFocus"

width_ :: Double -> Attr
width_ = mkStaticAttr "width"

height_ :: Double -> Attr
height_ = mkStaticAttr "height"

-- svg!

cx_ :: Double -> Attr
cx_ = mkStaticAttr "cx"

cy_ :: Double -> Attr
cy_ = mkStaticAttr "cy"

d_ :: Double -> Attr
d_ = mkStaticAttr "d"

dx_ :: Double -> Attr
dx_ = mkStaticAttr "dx"

dy_ :: Double -> Attr
dy_ = mkStaticAttr "dy"

x_ :: Double -> Attr
x_ = mkStaticAttr "x"

y_ :: Double -> Attr
y_ = mkStaticAttr "y"

r_ :: Double -> Attr
r_ = mkStaticAttr "r"

fill_ :: Text -> Attr
fill_ = mkStaticAttr "fill"

viewBox_ :: Text -> Attr
viewBox_ = mkStaticAttr "viewBox"

points_ :: Text -> Attr
points_ = mkStaticAttr "points"

transform_ :: Text -> Attr
transform_ = mkStaticAttr "transform"

-- fillOpacity fontFamily fontSize fx fy gradientTransform
-- gradientUnits markerEnd markerMid markerStart offset opacity
-- patternContentUnits patternUnits preserveAspectRatio r rx ry
-- spreadMethod stopColor stopOpacity stroke strokeDasharray strokeLinecap
-- strokeOpacity strokeWidth textAnchor version x1 x2 x y1 y2 y
