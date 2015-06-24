{-# LANGUAGE OverloadedStrings #-}
module React.DOM where

import React.Elements
import React.Imports
import React.Types

-- -- TODO ToJSString a => ?
-- -- Would this just be annoyingly ambiguous?
text_ :: JSString -> ReactNode sig
text_ = NodeText

-- TODO generate these automatically
a_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
a_ = domParent "a"

abbr_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
abbr_ = domParent "abbr"

address_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
address_ = domParent "address"

article_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
article_ = domParent "article"

aside_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
aside_ = domParent "aside"

audio_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
audio_ = domParent "audio"

b_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
b_ = domParent "b"

bdi_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
bdi_ = domParent "bdi"

bdo_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
bdo_ = domParent "bdo"

big_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
big_ = domParent "big"

blockquote_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
blockquote_ = domParent "blockquote"

body_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
body_ = domParent "body"

button_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
button_ = domParent "button"

canvas_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
canvas_ = domParent "canvas"

caption_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
caption_ = domParent "caption"

cite_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
cite_ = domParent "cite"

code_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
code_ = domParent "code"

colgroup_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
colgroup_ = domParent "colgroup"

data_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
data_ = domParent "data"

datalist_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
datalist_ = domParent "datalist"

dd_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
dd_ = domParent "dd"

del_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
del_ = domParent "del"

details_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
details_ = domParent "details"

dfn_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
dfn_ = domParent "dfn"

div_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
div_ = domParent "div"

dl_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
dl_ = domParent "dl"

dt_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
dt_ = domParent "dt"

em_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
em_ = domParent "em"

fieldset_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
fieldset_ = domParent "fieldset"

figcaption_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
figcaption_ = domParent "figcaption"

figure_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
figure_ = domParent "figure"

footer_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
footer_ = domParent "footer"

form_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
form_ = domParent "form"

h1_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
h1_ = domParent "h1"

h2_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
h2_ = domParent "h2"

h3_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
h3_ = domParent "h3"

h4_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
h4_ = domParent "h4"

h5_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
h5_ = domParent "h5"

h6_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
h6_ = domParent "h6"

head_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
head_ = domParent "head"

header_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
header_ = domParent "header"

html_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
html_ = domParent "html"

i_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
i_ = domParent "i"

iframe_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
iframe_ = domParent "iframe"

ins_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
ins_ = domParent "ins"

kbd_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
kbd_ = domParent "kbd"

label_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
label_ = domParent "label"

legend_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
legend_ = domParent "legend"

li_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
li_ = domParent "li"

main_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
main_ = domParent "main"

map_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
map_ = domParent "map"

mark_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
mark_ = domParent "mark"

menu_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
menu_ = domParent "menu"

menuitem_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
menuitem_ = domParent "menuitem"

meter_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
meter_ = domParent "meter"

nav_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
nav_ = domParent "nav"

noscript_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
noscript_ = domParent "noscript"

object_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
object_ = domParent "object"

ol_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
ol_ = domParent "ol"

optgroup_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
optgroup_ = domParent "optgroup"

option_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
option_ = domParent "option"

output_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
output_ = domParent "output"

p_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
p_ = domParent "p"

pre_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
pre_ = domParent "pre"

progress_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
progress_ = domParent "progress"

q_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
q_ = domParent "q"

rp_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
rp_ = domParent "rp"

rt_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
rt_ = domParent "rt"

ruby_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
ruby_ = domParent "ruby"

s_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
s_ = domParent "signal"

samp_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
samp_ = domParent "samp"

section_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
section_ = domParent "section"

select_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
select_ = domParent "select"

small_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
small_ = domParent "small"

span_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
span_ = domParent "span"

strong_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
strong_ = domParent "strong"

sub_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
sub_ = domParent "sub"

summary_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
summary_ = domParent "summary"

sup_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
sup_ = domParent "sup"

table_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
table_ = domParent "table"

tbody_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
tbody_ = domParent "tbody"

td_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
td_ = domParent "td"

tfoot_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
tfoot_ = domParent "tfoot"

th_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
th_ = domParent "th"

thead_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
thead_ = domParent "thead"

time_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
time_ = domParent "time"

tr_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
tr_ = domParent "tr"

u_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
u_ = domParent "u"

ul_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
ul_ = domParent "ul"

var_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
var_ = domParent "var"

video_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
video_ = domParent "video"


area_ :: [AttrOrHandler sig] -> ReactNode sig
area_ = domLeaf "area"

base_ :: [AttrOrHandler sig] -> ReactNode sig
base_ = domLeaf "base"

br_ :: [AttrOrHandler sig] -> ReactNode sig
br_ = domLeaf "br"

col_ :: [AttrOrHandler sig] -> ReactNode sig
col_ = domLeaf "col"

embed_ :: [AttrOrHandler sig] -> ReactNode sig
embed_ = domLeaf "embed"

hr_ :: [AttrOrHandler sig] -> ReactNode sig
hr_ = domLeaf "hr"

img_ :: [AttrOrHandler sig] -> ReactNode sig
img_ = domLeaf "img"

input_ :: [AttrOrHandler sig] -> ReactNode sig
input_ = domLeaf "input"

keygen_ :: [AttrOrHandler sig] -> ReactNode sig
keygen_ = domLeaf "keygen"

link_ :: [AttrOrHandler sig] -> ReactNode sig
link_ = domLeaf "link"

meta_ :: [AttrOrHandler sig] -> ReactNode sig
meta_ = domLeaf "meta"

param_ :: [AttrOrHandler sig] -> ReactNode sig
param_ = domLeaf "param"

source_ :: [AttrOrHandler sig] -> ReactNode sig
source_ = domLeaf "source"

track_ :: [AttrOrHandler sig] -> ReactNode sig
track_ = domLeaf "track"

wbr_ :: [AttrOrHandler sig] -> ReactNode sig
wbr_ = domLeaf "wbr"

-- script :: RawAttrs -> JSString -> IO ForeignNode
-- style :: RawAttrs -> JSString -> IO ForeignNode
-- textarea :: RawAttrs -> JSString -> IO ForeignNode
-- title :: RawAttrs -> JSString -> IO ForeignNode

-- svg!

svg_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
svg_ = domParent "svg"

defs_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
defs_ = domParent "defs"

g_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
g_ = domParent "g"

linearGradient_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
linearGradient_ = domParent "linearGradient"

mask_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
mask_ = domParent "mask"

pattern_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
pattern_ = domParent "pattern"

radialGradient_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
radialGradient_ = domParent "radialGradient"

stop_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
stop_ = domParent "stop"

-- text_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
-- text_ = domParent "text"

tspan_ :: [AttrOrHandler sig] -> ReactNode sig -> ReactNode sig
tspan_ = domParent "tspan"

circle_ :: [AttrOrHandler sig] -> ReactNode sig
circle_ = domLeaf "circle"

ellipse_ :: [AttrOrHandler sig] -> ReactNode sig
ellipse_ = domLeaf "ellipse"

line_ :: [AttrOrHandler sig] -> ReactNode sig
line_ = domLeaf "line"

path_ :: [AttrOrHandler sig] -> ReactNode sig
path_ = domLeaf "path"

polygon_ :: [AttrOrHandler sig] -> ReactNode sig
polygon_ = domLeaf "polygon"

polyline_ :: [AttrOrHandler sig] -> ReactNode sig
polyline_ = domLeaf "polyline"

rect_ :: [AttrOrHandler sig] -> ReactNode sig
rect_ = domLeaf "rect"
