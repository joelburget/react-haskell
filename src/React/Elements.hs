{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module React.Elements where

import Haste.Prim

import React.Types

import Prelude hiding (div, span)

text_ :: JSString -> React ty ()
text_ str = ReactT $ \_ -> return ([Text (fromJSStr str)], ())

a_ :: TermParent t => TermParentArg t -> t
a_ = termParent "a"
abbr_ :: TermParent t => TermParentArg t -> t
abbr_ = termParent "abbr"
address_ :: TermParent t => TermParentArg t -> t
address_ = termParent "address"
article_ :: TermParent t => TermParentArg t -> t
article_ = termParent "article"
aside_ :: TermParent t => TermParentArg t -> t
aside_ = termParent "aside"
audio_ :: TermParent t => TermParentArg t -> t
audio_ = termParent "audio"
b_ :: TermParent t => TermParentArg t -> t
b_ = termParent "b"
bdi_ :: TermParent t => TermParentArg t -> t
bdi_ = termParent "bdi"
bdo_ :: TermParent t => TermParentArg t -> t
bdo_ = termParent "bdo"
big_ :: TermParent t => TermParentArg t -> t
big_ = termParent "big"
blockquote_ :: TermParent t => TermParentArg t -> t
blockquote_ = termParent "blockquote"
body_ :: TermParent t => TermParentArg t -> t
body_ = termParent "body"
button_ :: TermParent t => TermParentArg t -> t
button_ = termParent "button"
canvas_ :: TermParent t => TermParentArg t -> t
canvas_ = termParent "canvas"
caption_ :: TermParent t => TermParentArg t -> t
caption_ = termParent "caption"
cite_ :: TermParent t => TermParentArg t -> t
cite_ = termParent "cite"
code_ :: TermParent t => TermParentArg t -> t
code_ = termParent "code"
colgroup_ :: TermParent t => TermParentArg t -> t
colgroup_ = termParent "colgroup"
data_ :: TermParent t => TermParentArg t -> t
data_ = termParent "data"
datalist_ :: TermParent t => TermParentArg t -> t
datalist_ = termParent "datalist"
dd_ :: TermParent t => TermParentArg t -> t
dd_ = termParent "dd"
del_ :: TermParent t => TermParentArg t -> t
del_ = termParent "del"
details_ :: TermParent t => TermParentArg t -> t
details_ = termParent "details"
dfn_ :: TermParent t => TermParentArg t -> t
dfn_ = termParent "dfn"
div_ :: TermParent t => TermParentArg t -> t
div_ = termParent "div"
dl_ :: TermParent t => TermParentArg t -> t
dl_ = termParent "dl"
dt_ :: TermParent t => TermParentArg t -> t
dt_ = termParent "dt"
em_ :: TermParent t => TermParentArg t -> t
em_ = termParent "em"
fieldset_ :: TermParent t => TermParentArg t -> t
fieldset_ = termParent "fieldset"
figcaption_ :: TermParent t => TermParentArg t -> t
figcaption_ = termParent "figcaption"
figure_ :: TermParent t => TermParentArg t -> t
figure_ = termParent "figure"
footer_ :: TermParent t => TermParentArg t -> t
footer_ = termParent "footer"
form_ :: TermParent t => TermParentArg t -> t
form_ = termParent "form"
h1_ :: TermParent t => TermParentArg t -> t
h1_ = termParent "h1"
h2_ :: TermParent t => TermParentArg t -> t
h2_ = termParent "h2"
h3_ :: TermParent t => TermParentArg t -> t
h3_ = termParent "h3"
h4_ :: TermParent t => TermParentArg t -> t
h4_ = termParent "h4"
h5_ :: TermParent t => TermParentArg t -> t
h5_ = termParent "h5"
h6_ :: TermParent t => TermParentArg t -> t
h6_ = termParent "h6"
head_ :: TermParent t => TermParentArg t -> t
head_ = termParent "head"
header_ :: TermParent t => TermParentArg t -> t
header_ = termParent "header"
html_ :: TermParent t => TermParentArg t -> t
html_ = termParent "html"
i_ :: TermParent t => TermParentArg t -> t
i_ = termParent "i"
iframe_ :: TermParent t => TermParentArg t -> t
iframe_ = termParent "iframe"
ins_ :: TermParent t => TermParentArg t -> t
ins_ = termParent "ins"
kbd_ :: TermParent t => TermParentArg t -> t
kbd_ = termParent "kbd"
label_ :: TermParent t => TermParentArg t -> t
label_ = termParent "label"
legend_ :: TermParent t => TermParentArg t -> t
legend_ = termParent "legend"
li_ :: TermParent t => TermParentArg t -> t
li_ = termParent "li"
main_ :: TermParent t => TermParentArg t -> t
main_ = termParent "main"
map_ :: TermParent t => TermParentArg t -> t
map_ = termParent "map"
mark_ :: TermParent t => TermParentArg t -> t
mark_ = termParent "mark"
menu_ :: TermParent t => TermParentArg t -> t
menu_ = termParent "menu"
menuitem_ :: TermParent t => TermParentArg t -> t
menuitem_ = termParent "menuitem"
meter_ :: TermParent t => TermParentArg t -> t
meter_ = termParent "meter"
nav_ :: TermParent t => TermParentArg t -> t
nav_ = termParent "nav"
noscript_ :: TermParent t => TermParentArg t -> t
noscript_ = termParent "noscript"
object_ :: TermParent t => TermParentArg t -> t
object_ = termParent "object"
ol_ :: TermParent t => TermParentArg t -> t
ol_ = termParent "ol"
optgroup_ :: TermParent t => TermParentArg t -> t
optgroup_ = termParent "optgroup"
option_ :: TermParent t => TermParentArg t -> t
option_ = termParent "option"
output_ :: TermParent t => TermParentArg t -> t
output_ = termParent "output"
p_ :: TermParent t => TermParentArg t -> t
p_ = termParent "p"
pre_ :: TermParent t => TermParentArg t -> t
pre_ = termParent "pre"
progress_ :: TermParent t => TermParentArg t -> t
progress_ = termParent "progress"
q_ :: TermParent t => TermParentArg t -> t
q_ = termParent "q"
rp_ :: TermParent t => TermParentArg t -> t
rp_ = termParent "rp"
rt_ :: TermParent t => TermParentArg t -> t
rt_ = termParent "rt"
ruby_ :: TermParent t => TermParentArg t -> t
ruby_ = termParent "ruby"
s_ :: TermParent t => TermParentArg t -> t
s_ = termParent "signal"
samp_ :: TermParent t => TermParentArg t -> t
samp_ = termParent "samp"
section_ :: TermParent t => TermParentArg t -> t
section_ = termParent "section"
select_ :: TermParent t => TermParentArg t -> t
select_ = termParent "select"
small_ :: TermParent t => TermParentArg t -> t
small_ = termParent "small"
span_ :: TermParent t => TermParentArg t -> t
span_ = termParent "span"
strong_ :: TermParent t => TermParentArg t -> t
strong_ = termParent "strong"
sub_ :: TermParent t => TermParentArg t -> t
sub_ = termParent "sub"
summary_ :: TermParent t => TermParentArg t -> t
summary_ = termParent "summary"
sup_ :: TermParent t => TermParentArg t -> t
sup_ = termParent "sup"
table_ :: TermParent t => TermParentArg t -> t
table_ = termParent "table"
tbody_ :: TermParent t => TermParentArg t -> t
tbody_ = termParent "tbody"
td_ :: TermParent t => TermParentArg t -> t
td_ = termParent "td"
tfoot_ :: TermParent t => TermParentArg t -> t
tfoot_ = termParent "tfoot"
th_ :: TermParent t => TermParentArg t -> t
th_ = termParent "th"
thead_ :: TermParent t => TermParentArg t -> t
thead_ = termParent "thead"
time_ :: TermParent t => TermParentArg t -> t
time_ = termParent "time"
tr_ :: TermParent t => TermParentArg t -> t
tr_ = termParent "tr"
u_ :: TermParent t => TermParentArg t -> t
u_ = termParent "u"
ul_ :: TermParent t => TermParentArg t -> t
ul_ = termParent "ul"
var_ :: TermParent t => TermParentArg t -> t
var_ = termParent "var"
video_ :: TermParent t => TermParentArg t -> t
video_ = termParent "video"

area_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
area_ = termLeaf "area"
base_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
base_ = termLeaf "base"
br_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
br_ = termLeaf "br"
col_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
col_ = termLeaf "col"
embed_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
embed_ = termLeaf "embed"
hr_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
hr_ = termLeaf "hr"
img_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
img_ = termLeaf "img"
input_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
input_ = termLeaf "input"
keygen_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
keygen_ = termLeaf "keygen"
link_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
link_ = termLeaf "link"
meta_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
meta_ = termLeaf "meta"
param_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
param_ = termLeaf "param"
source_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
source_ = termLeaf "source"
track_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
track_ = termLeaf "track"
wbr_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
wbr_ = termLeaf "wbr"

-- script :: RawAttrs -> JSString -> IO ForeignNode
-- style :: RawAttrs -> JSString -> IO ForeignNode
-- textarea :: RawAttrs -> JSString -> IO ForeignNode
-- title :: RawAttrs -> JSString -> IO ForeignNode

-- svg!

svg_ :: TermParent t => TermParentArg t -> t
svg_ = termParent "svg"

defs_ :: TermParent t => TermParentArg t -> t
defs_ = termParent "defs"

g_ :: TermParent t => TermParentArg t -> t
g_ = termParent "g"

linearGradient_ :: TermParent t => TermParentArg t -> t
linearGradient_ = termParent "linearGradient"

mask_ :: TermParent t => TermParentArg t -> t
mask_ = termParent "mask"

pattern_ :: TermParent t => TermParentArg t -> t
pattern_ = termParent "pattern"

radialGradient_ :: TermParent t => TermParentArg t -> t
radialGradient_ = termParent "radialGradient"

stop_ :: TermParent t => TermParentArg t -> t
stop_ = termParent "stop"

-- text_ :: TermParent t => TermParentArg t -> t
-- text_ = termParent "text"

tspan_ :: TermParent t => TermParentArg t -> t
tspan_ = termParent "tspan"

circle_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
circle_ = termLeaf "circle"

ellipse_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
ellipse_ = termLeaf "ellipse"

line_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
line_ = termLeaf "line"

path_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
path_ = termLeaf "path"

polygon_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
polygon_ = termLeaf "polygon"

polyline_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
polyline_ = termLeaf "polyline"

rect_ :: (Monad m, sig ~ Signal ty) => [AttrOrHandler sig] -> ReactT ty m ()
rect_ = termLeaf "rect"
