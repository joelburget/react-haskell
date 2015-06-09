{-# LANGUAGE OverloadedStrings, FlexibleInstances, DataKinds #-}
-- TODO(joel) rename to React.DOM?
module React.Elements where

import GHCJS.Foreign
import GHCJS.Types

import React.Imports
import React.Types


-- | Parent nodes always take a list of arguments and children.
-- @
-- span_ [class_ "example"] $ ... children ...
-- @
--
-- TODO questionable whether foreign nodes should use ReactBuiltin. Maybe
-- create a ReactForeign?
-- TODO(joel) this is essentially createElement
termParent :: ForeignRender
           -> [AttrOrHandler sig]
           -> ReactElement ty sig
           -> ReactElement RtBuiltin sig
termParent render attrs children =
    let (hs, as) = separateAttrs attrs
        childNodes = runReact children
    in ReactBuiltin [Static (Parent render as hs childNodes)]


foreignParent :: ForeignRender
              -> [AttrOrHandler sig]
              -> ReactElement ty sig
              -> ReactElement RtBuiltin sig
foreignParent = termParent


reactParent :: JSString
            -> [AttrOrHandler sig]
            -> ReactElement ty sig
            -> ReactElement RtBuiltin sig
reactParent name = termParent (js_React_DOM_parent name)


termLeaf :: ForeignRender
         -> [AttrOrHandler sig]
         -> ReactElement RtBuiltin sig
-- TODO questionable whether foreign nodes should use ReactBuiltin. Maybe
-- create a ReactForeign?
termLeaf render attrs =
    let (hs, as) = separateAttrs attrs
    in ReactBuiltin [Static (Leaf render as hs)]


foreignLeaf :: ForeignRender
            -> [AttrOrHandler sig]
            -> ReactElement RtBuiltin sig
foreignLeaf = termLeaf


reactLeaf :: JSString
         -> [AttrOrHandler sig]
         -> ReactElement RtBuiltin sig
reactLeaf name = termLeaf (\as' _ -> js_React_DOM_leaf name as')


-- TODO ToJSString a => ?
-- Would this just be annoyingly ambiguous?
text_ :: JSString -> ReactElement RtBuiltin sig
text_ str = ReactBuiltin $ [Static $ Text (fromJSString str)]

-- TODO generate these automatically
a_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
a_ = reactParent "a"

abbr_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
abbr_ = reactParent "abbr"

address_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
address_ = reactParent "address"

article_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
article_ = reactParent "article"

aside_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
aside_ = reactParent "aside"

audio_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
audio_ = reactParent "audio"

b_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
b_ = reactParent "b"

bdi_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
bdi_ = reactParent "bdi"

bdo_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
bdo_ = reactParent "bdo"

big_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
big_ = reactParent "big"

blockquote_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
blockquote_ = reactParent "blockquote"

body_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
body_ = reactParent "body"

button_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
button_ = reactParent "button"

canvas_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
canvas_ = reactParent "canvas"

caption_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
caption_ = reactParent "caption"

cite_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
cite_ = reactParent "cite"

code_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
code_ = reactParent "code"

colgroup_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
colgroup_ = reactParent "colgroup"

data_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
data_ = reactParent "data"

datalist_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
datalist_ = reactParent "datalist"

dd_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
dd_ = reactParent "dd"

del_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
del_ = reactParent "del"

details_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
details_ = reactParent "details"

dfn_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
dfn_ = reactParent "dfn"

div_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
div_ = reactParent "div"

dl_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
dl_ = reactParent "dl"

dt_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
dt_ = reactParent "dt"

em_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
em_ = reactParent "em"

fieldset_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
fieldset_ = reactParent "fieldset"

figcaption_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
figcaption_ = reactParent "figcaption"

figure_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
figure_ = reactParent "figure"

footer_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
footer_ = reactParent "footer"

form_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
form_ = reactParent "form"

h1_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
h1_ = reactParent "h1"

h2_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
h2_ = reactParent "h2"

h3_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
h3_ = reactParent "h3"

h4_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
h4_ = reactParent "h4"

h5_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
h5_ = reactParent "h5"

h6_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
h6_ = reactParent "h6"

head_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
head_ = reactParent "head"

header_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
header_ = reactParent "header"

html_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
html_ = reactParent "html"

i_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
i_ = reactParent "i"

iframe_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
iframe_ = reactParent "iframe"

ins_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
ins_ = reactParent "ins"

kbd_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
kbd_ = reactParent "kbd"

label_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
label_ = reactParent "label"

legend_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
legend_ = reactParent "legend"

li_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
li_ = reactParent "li"

main_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
main_ = reactParent "main"

map_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
map_ = reactParent "map"

mark_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
mark_ = reactParent "mark"

menu_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
menu_ = reactParent "menu"

menuitem_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
menuitem_ = reactParent "menuitem"

meter_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
meter_ = reactParent "meter"

nav_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
nav_ = reactParent "nav"

noscript_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
noscript_ = reactParent "noscript"

object_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
object_ = reactParent "object"

ol_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
ol_ = reactParent "ol"

optgroup_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
optgroup_ = reactParent "optgroup"

option_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
option_ = reactParent "option"

output_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
output_ = reactParent "output"

p_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
p_ = reactParent "p"

pre_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
pre_ = reactParent "pre"

progress_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
progress_ = reactParent "progress"

q_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
q_ = reactParent "q"

rp_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
rp_ = reactParent "rp"

rt_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
rt_ = reactParent "rt"

ruby_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
ruby_ = reactParent "ruby"

s_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
s_ = reactParent "signal"

samp_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
samp_ = reactParent "samp"

section_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
section_ = reactParent "section"

select_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
select_ = reactParent "select"

small_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
small_ = reactParent "small"

span_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
span_ = reactParent "span"

strong_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
strong_ = reactParent "strong"

sub_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
sub_ = reactParent "sub"

summary_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
summary_ = reactParent "summary"

sup_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
sup_ = reactParent "sup"

table_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
table_ = reactParent "table"

tbody_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
tbody_ = reactParent "tbody"

td_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
td_ = reactParent "td"

tfoot_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
tfoot_ = reactParent "tfoot"

th_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
th_ = reactParent "th"

thead_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
thead_ = reactParent "thead"

time_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
time_ = reactParent "time"

tr_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
tr_ = reactParent "tr"

u_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
u_ = reactParent "u"

ul_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
ul_ = reactParent "ul"

var_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
var_ = reactParent "var"

video_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
video_ = reactParent "video"


area_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
area_ = reactLeaf "area"

base_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
base_ = reactLeaf "base"

br_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
br_ = reactLeaf "br"

col_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
col_ = reactLeaf "col"

embed_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
embed_ = reactLeaf "embed"

hr_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
hr_ = reactLeaf "hr"

img_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
img_ = reactLeaf "img"

input_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
input_ = reactLeaf "input"

keygen_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
keygen_ = reactLeaf "keygen"

link_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
link_ = reactLeaf "link"

meta_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
meta_ = reactLeaf "meta"

param_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
param_ = reactLeaf "param"

source_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
source_ = reactLeaf "source"

track_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
track_ = reactLeaf "track"

wbr_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
wbr_ = reactLeaf "wbr"

-- script :: RawAttrs -> JSString -> IO ForeignNode
-- style :: RawAttrs -> JSString -> IO ForeignNode
-- textarea :: RawAttrs -> JSString -> IO ForeignNode
-- title :: RawAttrs -> JSString -> IO ForeignNode

-- svg!

svg_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
svg_ = reactParent "svg"

defs_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
defs_ = reactParent "defs"

g_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
g_ = reactParent "g"

linearGradient_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
linearGradient_ = reactParent "linearGradient"

mask_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
mask_ = reactParent "mask"

pattern_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
pattern_ = reactParent "pattern"

radialGradient_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
radialGradient_ = reactParent "radialGradient"

stop_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
stop_ = reactParent "stop"

-- text_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
-- text_ = reactParent "text"

tspan_ :: [AttrOrHandler sig] -> ReactElement ty sig -> ReactElement RtBuiltin sig
tspan_ = reactParent "tspan"

circle_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
circle_ = reactLeaf "circle"

ellipse_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
ellipse_ = reactLeaf "ellipse"

line_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
line_ = reactLeaf "line"

path_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
path_ = reactLeaf "path"

polygon_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
polygon_ = reactLeaf "polygon"

polyline_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
polyline_ = reactLeaf "polyline"

rect_ :: [AttrOrHandler sig] -> ReactElement RtBuiltin sig
rect_ = reactLeaf "rect"
