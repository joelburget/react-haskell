{-# LANGUAGE OverloadedStrings #-}
module React.Attrs where

import GHCJS.Types

import React.Types

class_ :: JSString -> AttrOrHandler signal
class_ = mkStaticAttr "className" Str

href_ :: JSString -> AttrOrHandler signal
href_ = mkStaticAttr "href" Str

id_ :: JSString -> AttrOrHandler signal
id_ = mkStaticAttr "id" Str

src_ :: JSString -> AttrOrHandler signal
src_ = mkStaticAttr "src" Str

style_ :: JSON -> AttrOrHandler signal
style_ = mkStaticAttr "style" id

value_ :: JSString -> AttrOrHandler signal
value_ = mkStaticAttr "value" Str

placeholder_ :: JSString -> AttrOrHandler signal
placeholder_ = mkStaticAttr "placeholder" Str

for_ :: JSString -> AttrOrHandler signal
for_ = mkStaticAttr "htmlFor" Str

type_ :: JSString -> AttrOrHandler signal
type_ = mkStaticAttr "type" Str

checked_ :: Bool -> AttrOrHandler signal
checked_ = mkStaticAttr "checked" Bool

autofocus_ :: Bool -> AttrOrHandler signal
autofocus_ = mkStaticAttr "autoFocus" Bool

width_ :: Double -> AttrOrHandler signal
width_ = mkStaticAttr "width" Num

height_ :: Double -> AttrOrHandler signal
height_ = mkStaticAttr "height" Num

-- svg!

cx_ :: Double -> AttrOrHandler signal
cx_ = mkStaticAttr "cx" Num

cy_ :: Double -> AttrOrHandler signal
cy_ = mkStaticAttr "cy" Num

d_ :: Double -> AttrOrHandler signal
d_ = mkStaticAttr "d" Num

dx_ :: Double -> AttrOrHandler signal
dx_ = mkStaticAttr "dx" Num

dy_ :: Double -> AttrOrHandler signal
dy_ = mkStaticAttr "dy" Num

x_ :: Double -> AttrOrHandler signal
x_ = mkStaticAttr "x" Num

y_ :: Double -> AttrOrHandler signal
y_ = mkStaticAttr "y" Num

r_ :: Double -> AttrOrHandler signal
r_ = mkStaticAttr "r" Num

fill_ :: JSString -> AttrOrHandler signal
fill_ = mkStaticAttr "fill" Str

viewBox_ :: JSString -> AttrOrHandler signal
viewBox_ = mkStaticAttr "viewBox" Str

points_ :: JSString -> AttrOrHandler signal
points_ = mkStaticAttr "points" Str

transform_ :: JSString -> AttrOrHandler signal
transform_ = mkStaticAttr "transform" Str

multiple_ :: Bool -> AttrOrHandler signal
multiple_ = mkStaticAttr "multiple" Bool

-- fillOpacity fontFamily fontSize fx fy gradientTransform
-- gradientUnits markerEnd markerMid markerStart offset opacity
-- patternContentUnits patternUnits preserveAspectRatio r rx ry
-- spreadMethod stopColor stopOpacity stroke strokeDasharray strokeLinecap
-- strokeOpacity strokeWidth textAnchor version x1 x2 x y1 y2 y
