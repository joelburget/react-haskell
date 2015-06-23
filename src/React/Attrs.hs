{-# LANGUAGE OverloadedStrings #-}
module React.Attrs where

import qualified Data.Aeson as Aeson
import Data.Text (Text)

import React.Types


mkStaticAttr :: Aeson.ToJSON a => Text -> a -> AttrOrHandler sig
mkStaticAttr name = StaticAttr name . Aeson.toJSON


key_ :: Text -> AttrOrHandler sig
key_ = mkStaticAttr "key"

class_ :: Text -> AttrOrHandler sig
class_ = mkStaticAttr "className"

href_ :: Text -> AttrOrHandler sig
href_ = mkStaticAttr "href"

id_ :: Text -> AttrOrHandler sig
id_ = mkStaticAttr "id"

src_ :: Text -> AttrOrHandler sig
src_ = mkStaticAttr "src"

style_ :: JSON -> AttrOrHandler sig
style_ = mkStaticAttr "style"

value_ :: Text -> AttrOrHandler sig
value_ = mkStaticAttr "value"

placeholder_ :: Text -> AttrOrHandler sig
placeholder_ = mkStaticAttr "placeholder"

for_ :: Text -> AttrOrHandler sig
for_ = mkStaticAttr "htmlFor"

type_ :: Text -> AttrOrHandler sig
type_ = mkStaticAttr "type"

checked_ :: Bool -> AttrOrHandler sig
checked_ = mkStaticAttr "checked"

autofocus_ :: Bool -> AttrOrHandler sig
autofocus_ = mkStaticAttr "autoFocus"

width_ :: Double -> AttrOrHandler sig
width_ = mkStaticAttr "width"

height_ :: Double -> AttrOrHandler sig
height_ = mkStaticAttr "height"

-- svg!

cx_ :: Double -> AttrOrHandler sig
cx_ = mkStaticAttr "cx"

cy_ :: Double -> AttrOrHandler sig
cy_ = mkStaticAttr "cy"

d_ :: Double -> AttrOrHandler sig
d_ = mkStaticAttr "d"

dx_ :: Double -> AttrOrHandler sig
dx_ = mkStaticAttr "dx"

dy_ :: Double -> AttrOrHandler sig
dy_ = mkStaticAttr "dy"

x_ :: Double -> AttrOrHandler sig
x_ = mkStaticAttr "x"

y_ :: Double -> AttrOrHandler sig
y_ = mkStaticAttr "y"

r_ :: Double -> AttrOrHandler sig
r_ = mkStaticAttr "r"

fill_ :: Text -> AttrOrHandler sig
fill_ = mkStaticAttr "fill"

viewBox_ :: Text -> AttrOrHandler sig
viewBox_ = mkStaticAttr "viewBox"

points_ :: Text -> AttrOrHandler sig
points_ = mkStaticAttr "points"

transform_ :: Text -> AttrOrHandler sig
transform_ = mkStaticAttr "transform"

-- fillOpacity fontFamily fontSize fx fy gradientTransform
-- gradientUnits markerEnd markerMid markerStart offset opacity
-- patternContentUnits patternUnits preserveAspectRatio r rx ry
-- spreadMethod stopColor stopOpacity stroke strokeDasharray strokeLinecap
-- strokeOpacity strokeWidth textAnchor version x1 x2 x y1 y2 y
