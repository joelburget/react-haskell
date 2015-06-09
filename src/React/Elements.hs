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
           -> React ty sig
           -> React RtBuiltin sig
termParent render attrs children =
    let (hs, as) = separateAttrs attrs
        childNodes = runReact children
    in ReactBuiltin [Static (Parent render as hs childNodes)]


foreignParent :: ForeignRender
              -> [AttrOrHandler sig]
              -> React ty sig
              -> React RtBuiltin sig
foreignParent = termParent


reactParent :: JSString
            -> [AttrOrHandler sig]
            -> React ty sig
            -> React RtBuiltin sig
reactParent name = termParent (js_React_DOM_parent name)


termLeaf :: ForeignRender
         -> [AttrOrHandler sig]
         -> React RtBuiltin sig
-- TODO questionable whether foreign nodes should use ReactBuiltin. Maybe
-- create a ReactForeign?
termLeaf render attrs =
    let (hs, as) = separateAttrs attrs
    in ReactBuiltin [Static (Leaf render as hs)]


foreignLeaf :: ForeignRender
            -> [AttrOrHandler sig]
            -> React RtBuiltin sig
foreignLeaf = termLeaf


reactLeaf :: JSString
         -> [AttrOrHandler sig]
         -> React RtBuiltin sig
reactLeaf name = termLeaf (\as' _ -> js_React_DOM_leaf name as')


-- TODO ToJSString a => ?
-- Would this just be annoyingly ambiguous?
text_ :: JSString -> React RtBuiltin sig
text_ str = ReactBuiltin $ [Static $ Text (fromJSString str)]

-- TODO generate these automatically
a_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
a_ = reactParent "a"

abbr_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
abbr_ = reactParent "abbr"

address_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
address_ = reactParent "address"

article_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
article_ = reactParent "article"

aside_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
aside_ = reactParent "aside"

audio_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
audio_ = reactParent "audio"

b_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
b_ = reactParent "b"

bdi_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
bdi_ = reactParent "bdi"

bdo_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
bdo_ = reactParent "bdo"

big_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
big_ = reactParent "big"

blockquote_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
blockquote_ = reactParent "blockquote"

body_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
body_ = reactParent "body"

button_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
button_ = reactParent "button"

canvas_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
canvas_ = reactParent "canvas"

caption_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
caption_ = reactParent "caption"

cite_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
cite_ = reactParent "cite"

code_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
code_ = reactParent "code"

colgroup_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
colgroup_ = reactParent "colgroup"

data_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
data_ = reactParent "data"

datalist_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
datalist_ = reactParent "datalist"

dd_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
dd_ = reactParent "dd"

del_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
del_ = reactParent "del"

details_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
details_ = reactParent "details"

dfn_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
dfn_ = reactParent "dfn"

div_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
div_ = reactParent "div"

dl_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
dl_ = reactParent "dl"

dt_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
dt_ = reactParent "dt"

em_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
em_ = reactParent "em"

fieldset_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
fieldset_ = reactParent "fieldset"

figcaption_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
figcaption_ = reactParent "figcaption"

figure_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
figure_ = reactParent "figure"

footer_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
footer_ = reactParent "footer"

form_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
form_ = reactParent "form"

h1_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
h1_ = reactParent "h1"

h2_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
h2_ = reactParent "h2"

h3_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
h3_ = reactParent "h3"

h4_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
h4_ = reactParent "h4"

h5_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
h5_ = reactParent "h5"

h6_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
h6_ = reactParent "h6"

head_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
head_ = reactParent "head"

header_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
header_ = reactParent "header"

html_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
html_ = reactParent "html"

i_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
i_ = reactParent "i"

iframe_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
iframe_ = reactParent "iframe"

ins_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
ins_ = reactParent "ins"

kbd_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
kbd_ = reactParent "kbd"

label_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
label_ = reactParent "label"

legend_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
legend_ = reactParent "legend"

li_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
li_ = reactParent "li"

main_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
main_ = reactParent "main"

map_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
map_ = reactParent "map"

mark_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
mark_ = reactParent "mark"

menu_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
menu_ = reactParent "menu"

menuitem_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
menuitem_ = reactParent "menuitem"

meter_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
meter_ = reactParent "meter"

nav_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
nav_ = reactParent "nav"

noscript_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
noscript_ = reactParent "noscript"

object_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
object_ = reactParent "object"

ol_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
ol_ = reactParent "ol"

optgroup_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
optgroup_ = reactParent "optgroup"

option_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
option_ = reactParent "option"

output_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
output_ = reactParent "output"

p_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
p_ = reactParent "p"

pre_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
pre_ = reactParent "pre"

progress_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
progress_ = reactParent "progress"

q_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
q_ = reactParent "q"

rp_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
rp_ = reactParent "rp"

rt_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
rt_ = reactParent "rt"

ruby_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
ruby_ = reactParent "ruby"

s_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
s_ = reactParent "signal"

samp_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
samp_ = reactParent "samp"

section_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
section_ = reactParent "section"

select_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
select_ = reactParent "select"

small_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
small_ = reactParent "small"

span_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
span_ = reactParent "span"

strong_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
strong_ = reactParent "strong"

sub_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
sub_ = reactParent "sub"

summary_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
summary_ = reactParent "summary"

sup_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
sup_ = reactParent "sup"

table_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
table_ = reactParent "table"

tbody_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
tbody_ = reactParent "tbody"

td_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
td_ = reactParent "td"

tfoot_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
tfoot_ = reactParent "tfoot"

th_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
th_ = reactParent "th"

thead_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
thead_ = reactParent "thead"

time_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
time_ = reactParent "time"

tr_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
tr_ = reactParent "tr"

u_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
u_ = reactParent "u"

ul_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
ul_ = reactParent "ul"

var_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
var_ = reactParent "var"

video_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
video_ = reactParent "video"


area_ :: [AttrOrHandler sig] -> React RtBuiltin sig
area_ = reactLeaf "area"

base_ :: [AttrOrHandler sig] -> React RtBuiltin sig
base_ = reactLeaf "base"

br_ :: [AttrOrHandler sig] -> React RtBuiltin sig
br_ = reactLeaf "br"

col_ :: [AttrOrHandler sig] -> React RtBuiltin sig
col_ = reactLeaf "col"

embed_ :: [AttrOrHandler sig] -> React RtBuiltin sig
embed_ = reactLeaf "embed"

hr_ :: [AttrOrHandler sig] -> React RtBuiltin sig
hr_ = reactLeaf "hr"

img_ :: [AttrOrHandler sig] -> React RtBuiltin sig
img_ = reactLeaf "img"

input_ :: [AttrOrHandler sig] -> React RtBuiltin sig
input_ = reactLeaf "input"

keygen_ :: [AttrOrHandler sig] -> React RtBuiltin sig
keygen_ = reactLeaf "keygen"

link_ :: [AttrOrHandler sig] -> React RtBuiltin sig
link_ = reactLeaf "link"

meta_ :: [AttrOrHandler sig] -> React RtBuiltin sig
meta_ = reactLeaf "meta"

param_ :: [AttrOrHandler sig] -> React RtBuiltin sig
param_ = reactLeaf "param"

source_ :: [AttrOrHandler sig] -> React RtBuiltin sig
source_ = reactLeaf "source"

track_ :: [AttrOrHandler sig] -> React RtBuiltin sig
track_ = reactLeaf "track"

wbr_ :: [AttrOrHandler sig] -> React RtBuiltin sig
wbr_ = reactLeaf "wbr"

-- script :: RawAttrs -> JSString -> IO ForeignNode
-- style :: RawAttrs -> JSString -> IO ForeignNode
-- textarea :: RawAttrs -> JSString -> IO ForeignNode
-- title :: RawAttrs -> JSString -> IO ForeignNode

-- svg!

svg_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
svg_ = reactParent "svg"

defs_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
defs_ = reactParent "defs"

g_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
g_ = reactParent "g"

linearGradient_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
linearGradient_ = reactParent "linearGradient"

mask_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
mask_ = reactParent "mask"

pattern_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
pattern_ = reactParent "pattern"

radialGradient_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
radialGradient_ = reactParent "radialGradient"

stop_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
stop_ = reactParent "stop"

-- text_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
-- text_ = reactParent "text"

tspan_ :: [AttrOrHandler sig] -> React ty sig -> React RtBuiltin sig
tspan_ = reactParent "tspan"

circle_ :: [AttrOrHandler sig] -> React RtBuiltin sig
circle_ = reactLeaf "circle"

ellipse_ :: [AttrOrHandler sig] -> React RtBuiltin sig
ellipse_ = reactLeaf "ellipse"

line_ :: [AttrOrHandler sig] -> React RtBuiltin sig
line_ = reactLeaf "line"

path_ :: [AttrOrHandler sig] -> React RtBuiltin sig
path_ = reactLeaf "path"

polygon_ :: [AttrOrHandler sig] -> React RtBuiltin sig
polygon_ = reactLeaf "polygon"

polyline_ :: [AttrOrHandler sig] -> React RtBuiltin sig
polyline_ = reactLeaf "polyline"

rect_ :: [AttrOrHandler sig] -> React RtBuiltin sig
rect_ = reactLeaf "rect"
