{-# LANGUAGE OverloadedStrings, FlexibleInstances, DataKinds #-}
-- TODO(joel) rename to React.DOM?
module React.Elements where

import Data.Aeson as Aeson
import Data.String
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import React.Imports
import React.Types


attrsToJson :: [Attr] -> JSON
attrsToJson = Aeson.toJSON . map unAttr where
    unAttr (Attr name json) = (name, json)


attrsToJSAny :: [Attr] -> IO JSAny
attrsToJSAny attrs = castRef <$> toJSRef (attrsToJson attrs)

-- | Parent nodes always take a list of arguments and children.
-- @
-- span_ [class_ "example"] $ ... children ...
-- @
--
-- TODO questionable whether foreign nodes should use ReactBuiltin. Maybe
-- create a ReactForeign?
-- TODO(joel) this is essentially createElement
domParent :: JSString
          -> [Attr]
          -> ReactNode sig
          -> ReactNode sig
domParent name attrs children =
    DomElement (ReactDOMElement name (attrsToJson attrs) children "" Nothing)


classParent :: ReactClass props state sig
            -- -> [Attr]
            -> ReactNode sig
            -> props
            -> ReactNode sig
classParent (ReactClass cls) children props =
    ComponentElement (ReactComponentElement cls (attrsToJSAny []) children "" Nothing)


domLeaf :: JSString
        -> [Attr]
        -> ReactNode sig
domLeaf name attrs =
    DomElement (ReactDOMElement name (attrsToJson attrs) mempty "" Nothing)


classLeaf :: ReactClass props state sig
          -- -> [Attr]
          -> props
          -> ReactNode sig
classLeaf (ReactClass cls) props =
    ComponentElement (ReactComponentElement cls (attrsToJSAny []) mempty "" Nothing)


-- -- TODO ToJSString a => ?
-- -- Would this just be annoyingly ambiguous?
text_ :: String -> ReactNode sig
text_ = fromString

-- TODO generate these automatically
a_ :: [Attr] -> ReactNode sig -> ReactNode sig
a_ = domParent "a"

abbr_ :: [Attr] -> ReactNode sig -> ReactNode sig
abbr_ = domParent "abbr"

address_ :: [Attr] -> ReactNode sig -> ReactNode sig
address_ = domParent "address"

article_ :: [Attr] -> ReactNode sig -> ReactNode sig
article_ = domParent "article"

aside_ :: [Attr] -> ReactNode sig -> ReactNode sig
aside_ = domParent "aside"

audio_ :: [Attr] -> ReactNode sig -> ReactNode sig
audio_ = domParent "audio"

b_ :: [Attr] -> ReactNode sig -> ReactNode sig
b_ = domParent "b"

bdi_ :: [Attr] -> ReactNode sig -> ReactNode sig
bdi_ = domParent "bdi"

bdo_ :: [Attr] -> ReactNode sig -> ReactNode sig
bdo_ = domParent "bdo"

big_ :: [Attr] -> ReactNode sig -> ReactNode sig
big_ = domParent "big"

blockquote_ :: [Attr] -> ReactNode sig -> ReactNode sig
blockquote_ = domParent "blockquote"

body_ :: [Attr] -> ReactNode sig -> ReactNode sig
body_ = domParent "body"

button_ :: [Attr] -> ReactNode sig -> ReactNode sig
button_ = domParent "button"

canvas_ :: [Attr] -> ReactNode sig -> ReactNode sig
canvas_ = domParent "canvas"

caption_ :: [Attr] -> ReactNode sig -> ReactNode sig
caption_ = domParent "caption"

cite_ :: [Attr] -> ReactNode sig -> ReactNode sig
cite_ = domParent "cite"

code_ :: [Attr] -> ReactNode sig -> ReactNode sig
code_ = domParent "code"

colgroup_ :: [Attr] -> ReactNode sig -> ReactNode sig
colgroup_ = domParent "colgroup"

data_ :: [Attr] -> ReactNode sig -> ReactNode sig
data_ = domParent "data"

datalist_ :: [Attr] -> ReactNode sig -> ReactNode sig
datalist_ = domParent "datalist"

dd_ :: [Attr] -> ReactNode sig -> ReactNode sig
dd_ = domParent "dd"

del_ :: [Attr] -> ReactNode sig -> ReactNode sig
del_ = domParent "del"

details_ :: [Attr] -> ReactNode sig -> ReactNode sig
details_ = domParent "details"

dfn_ :: [Attr] -> ReactNode sig -> ReactNode sig
dfn_ = domParent "dfn"

div_ :: [Attr] -> ReactNode sig -> ReactNode sig
div_ = domParent "div"

dl_ :: [Attr] -> ReactNode sig -> ReactNode sig
dl_ = domParent "dl"

dt_ :: [Attr] -> ReactNode sig -> ReactNode sig
dt_ = domParent "dt"

em_ :: [Attr] -> ReactNode sig -> ReactNode sig
em_ = domParent "em"

fieldset_ :: [Attr] -> ReactNode sig -> ReactNode sig
fieldset_ = domParent "fieldset"

figcaption_ :: [Attr] -> ReactNode sig -> ReactNode sig
figcaption_ = domParent "figcaption"

figure_ :: [Attr] -> ReactNode sig -> ReactNode sig
figure_ = domParent "figure"

footer_ :: [Attr] -> ReactNode sig -> ReactNode sig
footer_ = domParent "footer"

form_ :: [Attr] -> ReactNode sig -> ReactNode sig
form_ = domParent "form"

h1_ :: [Attr] -> ReactNode sig -> ReactNode sig
h1_ = domParent "h1"

h2_ :: [Attr] -> ReactNode sig -> ReactNode sig
h2_ = domParent "h2"

h3_ :: [Attr] -> ReactNode sig -> ReactNode sig
h3_ = domParent "h3"

h4_ :: [Attr] -> ReactNode sig -> ReactNode sig
h4_ = domParent "h4"

h5_ :: [Attr] -> ReactNode sig -> ReactNode sig
h5_ = domParent "h5"

h6_ :: [Attr] -> ReactNode sig -> ReactNode sig
h6_ = domParent "h6"

head_ :: [Attr] -> ReactNode sig -> ReactNode sig
head_ = domParent "head"

header_ :: [Attr] -> ReactNode sig -> ReactNode sig
header_ = domParent "header"

html_ :: [Attr] -> ReactNode sig -> ReactNode sig
html_ = domParent "html"

i_ :: [Attr] -> ReactNode sig -> ReactNode sig
i_ = domParent "i"

iframe_ :: [Attr] -> ReactNode sig -> ReactNode sig
iframe_ = domParent "iframe"

ins_ :: [Attr] -> ReactNode sig -> ReactNode sig
ins_ = domParent "ins"

kbd_ :: [Attr] -> ReactNode sig -> ReactNode sig
kbd_ = domParent "kbd"

label_ :: [Attr] -> ReactNode sig -> ReactNode sig
label_ = domParent "label"

legend_ :: [Attr] -> ReactNode sig -> ReactNode sig
legend_ = domParent "legend"

li_ :: [Attr] -> ReactNode sig -> ReactNode sig
li_ = domParent "li"

main_ :: [Attr] -> ReactNode sig -> ReactNode sig
main_ = domParent "main"

map_ :: [Attr] -> ReactNode sig -> ReactNode sig
map_ = domParent "map"

mark_ :: [Attr] -> ReactNode sig -> ReactNode sig
mark_ = domParent "mark"

menu_ :: [Attr] -> ReactNode sig -> ReactNode sig
menu_ = domParent "menu"

menuitem_ :: [Attr] -> ReactNode sig -> ReactNode sig
menuitem_ = domParent "menuitem"

meter_ :: [Attr] -> ReactNode sig -> ReactNode sig
meter_ = domParent "meter"

nav_ :: [Attr] -> ReactNode sig -> ReactNode sig
nav_ = domParent "nav"

noscript_ :: [Attr] -> ReactNode sig -> ReactNode sig
noscript_ = domParent "noscript"

object_ :: [Attr] -> ReactNode sig -> ReactNode sig
object_ = domParent "object"

ol_ :: [Attr] -> ReactNode sig -> ReactNode sig
ol_ = domParent "ol"

optgroup_ :: [Attr] -> ReactNode sig -> ReactNode sig
optgroup_ = domParent "optgroup"

option_ :: [Attr] -> ReactNode sig -> ReactNode sig
option_ = domParent "option"

output_ :: [Attr] -> ReactNode sig -> ReactNode sig
output_ = domParent "output"

p_ :: [Attr] -> ReactNode sig -> ReactNode sig
p_ = domParent "p"

pre_ :: [Attr] -> ReactNode sig -> ReactNode sig
pre_ = domParent "pre"

progress_ :: [Attr] -> ReactNode sig -> ReactNode sig
progress_ = domParent "progress"

q_ :: [Attr] -> ReactNode sig -> ReactNode sig
q_ = domParent "q"

rp_ :: [Attr] -> ReactNode sig -> ReactNode sig
rp_ = domParent "rp"

rt_ :: [Attr] -> ReactNode sig -> ReactNode sig
rt_ = domParent "rt"

ruby_ :: [Attr] -> ReactNode sig -> ReactNode sig
ruby_ = domParent "ruby"

s_ :: [Attr] -> ReactNode sig -> ReactNode sig
s_ = domParent "signal"

samp_ :: [Attr] -> ReactNode sig -> ReactNode sig
samp_ = domParent "samp"

section_ :: [Attr] -> ReactNode sig -> ReactNode sig
section_ = domParent "section"

select_ :: [Attr] -> ReactNode sig -> ReactNode sig
select_ = domParent "select"

small_ :: [Attr] -> ReactNode sig -> ReactNode sig
small_ = domParent "small"

span_ :: [Attr] -> ReactNode sig -> ReactNode sig
span_ = domParent "span"

strong_ :: [Attr] -> ReactNode sig -> ReactNode sig
strong_ = domParent "strong"

sub_ :: [Attr] -> ReactNode sig -> ReactNode sig
sub_ = domParent "sub"

summary_ :: [Attr] -> ReactNode sig -> ReactNode sig
summary_ = domParent "summary"

sup_ :: [Attr] -> ReactNode sig -> ReactNode sig
sup_ = domParent "sup"

table_ :: [Attr] -> ReactNode sig -> ReactNode sig
table_ = domParent "table"

tbody_ :: [Attr] -> ReactNode sig -> ReactNode sig
tbody_ = domParent "tbody"

td_ :: [Attr] -> ReactNode sig -> ReactNode sig
td_ = domParent "td"

tfoot_ :: [Attr] -> ReactNode sig -> ReactNode sig
tfoot_ = domParent "tfoot"

th_ :: [Attr] -> ReactNode sig -> ReactNode sig
th_ = domParent "th"

thead_ :: [Attr] -> ReactNode sig -> ReactNode sig
thead_ = domParent "thead"

time_ :: [Attr] -> ReactNode sig -> ReactNode sig
time_ = domParent "time"

tr_ :: [Attr] -> ReactNode sig -> ReactNode sig
tr_ = domParent "tr"

u_ :: [Attr] -> ReactNode sig -> ReactNode sig
u_ = domParent "u"

ul_ :: [Attr] -> ReactNode sig -> ReactNode sig
ul_ = domParent "ul"

var_ :: [Attr] -> ReactNode sig -> ReactNode sig
var_ = domParent "var"

video_ :: [Attr] -> ReactNode sig -> ReactNode sig
video_ = domParent "video"


area_ :: [Attr] -> ReactNode sig
area_ = domLeaf "area"

base_ :: [Attr] -> ReactNode sig
base_ = domLeaf "base"

br_ :: [Attr] -> ReactNode sig
br_ = domLeaf "br"

col_ :: [Attr] -> ReactNode sig
col_ = domLeaf "col"

embed_ :: [Attr] -> ReactNode sig
embed_ = domLeaf "embed"

hr_ :: [Attr] -> ReactNode sig
hr_ = domLeaf "hr"

img_ :: [Attr] -> ReactNode sig
img_ = domLeaf "img"

input_ :: [Attr] -> ReactNode sig
input_ = domLeaf "input"

keygen_ :: [Attr] -> ReactNode sig
keygen_ = domLeaf "keygen"

link_ :: [Attr] -> ReactNode sig
link_ = domLeaf "link"

meta_ :: [Attr] -> ReactNode sig
meta_ = domLeaf "meta"

param_ :: [Attr] -> ReactNode sig
param_ = domLeaf "param"

source_ :: [Attr] -> ReactNode sig
source_ = domLeaf "source"

track_ :: [Attr] -> ReactNode sig
track_ = domLeaf "track"

wbr_ :: [Attr] -> ReactNode sig
wbr_ = domLeaf "wbr"

-- script :: RawAttrs -> JSString -> IO ForeignNode
-- style :: RawAttrs -> JSString -> IO ForeignNode
-- textarea :: RawAttrs -> JSString -> IO ForeignNode
-- title :: RawAttrs -> JSString -> IO ForeignNode

-- svg!

svg_ :: [Attr] -> ReactNode sig -> ReactNode sig
svg_ = domParent "svg"

defs_ :: [Attr] -> ReactNode sig -> ReactNode sig
defs_ = domParent "defs"

g_ :: [Attr] -> ReactNode sig -> ReactNode sig
g_ = domParent "g"

linearGradient_ :: [Attr] -> ReactNode sig -> ReactNode sig
linearGradient_ = domParent "linearGradient"

mask_ :: [Attr] -> ReactNode sig -> ReactNode sig
mask_ = domParent "mask"

pattern_ :: [Attr] -> ReactNode sig -> ReactNode sig
pattern_ = domParent "pattern"

radialGradient_ :: [Attr] -> ReactNode sig -> ReactNode sig
radialGradient_ = domParent "radialGradient"

stop_ :: [Attr] -> ReactNode sig -> ReactNode sig
stop_ = domParent "stop"

-- text_ :: [Attr] -> ReactNode sig -> ReactNode sig
-- text_ = domParent "text"

tspan_ :: [Attr] -> ReactNode sig -> ReactNode sig
tspan_ = domParent "tspan"

circle_ :: [Attr] -> ReactNode sig
circle_ = domLeaf "circle"

ellipse_ :: [Attr] -> ReactNode sig
ellipse_ = domLeaf "ellipse"

line_ :: [Attr] -> ReactNode sig
line_ = domLeaf "line"

path_ :: [Attr] -> ReactNode sig
path_ = domLeaf "path"

polygon_ :: [Attr] -> ReactNode sig
polygon_ = domLeaf "polygon"

polyline_ :: [Attr] -> ReactNode sig
polyline_ = domLeaf "polyline"

rect_ :: [Attr] -> ReactNode sig
rect_ = domLeaf "rect"
