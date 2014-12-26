{-# LANGUAGE OverloadedStrings #-}
module React.Attrs where

import Haste.JSON
import Haste.Prim

-- class ReactAttr a where

type Attr = (JSString, JSON)

mkAttr :: JSString -> (a -> JSON) -> a -> Attr
mkAttr name f a = (name, f a)

-- exception
className :: JSString -> Attr
className = mkAttr "className" Str

class_ :: JSString -> Attr
class_ = mkAttr "className" Str

href_ :: JSString -> Attr
href_ = mkAttr "href" Str

id_ :: JSString -> Attr
id_ = mkAttr "id" Str

src_ :: JSString -> Attr
src_ = mkAttr "src" Str

style_ :: JSON -> Attr
style_ = mkAttr "style" id

value_ :: JSString -> Attr
value_ = mkAttr "value" Str

placeholder_ :: JSString -> Attr
placeholder_ = mkAttr "placeholder" Str

for_ :: JSString -> Attr
for_ = mkAttr "htmlFor" Str

type_ :: JSString -> Attr
type_ = mkAttr "type" Str

checked_ :: Bool -> Attr
checked_ = mkAttr "checked" Bool

-- TODO think about supporting autoFocus / other camel-cased react names
autofocus_ :: Bool -> Attr
autofocus_ = mkAttr "autoFocus" Bool

width_ :: Double -> Attr
width_ = mkAttr "width" Num

height_ :: Double -> Attr
height_ = mkAttr "height" Num

-- svg!

cx_ :: Double -> Attr
cx_ = mkAttr "cx" Num

cy_ :: Double -> Attr
cy_ = mkAttr "cy" Num

d_ :: Double -> Attr
d_ = mkAttr "d" Num

dx_ :: Double -> Attr
dx_ = mkAttr "dx" Num

dy_ :: Double -> Attr
dy_ = mkAttr "dy" Num

x_ :: Double -> Attr
x_ = mkAttr "x" Num

y_ :: Double -> Attr
y_ = mkAttr "y" Num

r_ :: Double -> Attr
r_ = mkAttr "r" Num

fill_ :: JSString -> Attr
fill_ = mkAttr "fill" Str

viewBox_ :: JSString -> Attr
viewBox_ = mkAttr "viewBox" Str

points_ :: JSString -> Attr
points_ = mkAttr "points" Str

transform_ :: JSString -> Attr
transform_ = mkAttr "transform" Str

-- fillOpacity fontFamily fontSize fx fy gradientTransform
-- gradientUnits markerEnd markerMid markerStart offset opacity
-- patternContentUnits patternUnits preserveAspectRatio r rx ry
-- spreadMethod stopColor stopOpacity stroke strokeDasharray strokeLinecap
-- strokeOpacity strokeWidth textAnchor version x1 x2 x y1 y2 y
