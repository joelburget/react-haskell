{-# LANGUAGE OverloadedStrings #-}
module React.Elements where

import Haste.Prim

import React.Types

import Prelude hiding (div, span)

makeParent :: Monad m
           => JSString
           -> ReactT anim signal m a
           -> ReactT anim signal m ()
makeParent name children = ReactT $ \anim -> do
    ~(childNodes, _) <- runReactT children anim
    return ([Parent name [] [] childNodes], ())

makeLeaf :: Monad m
         => JSString
         -> ReactT anim signal m ()
makeLeaf name = ReactT $ \_ -> return ([Leaf name [] []], ())

text_ :: JSString -> React anim signal ()
text_ str = ReactT $ \_ -> return ([Text (fromJSStr str)], ())

a_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
a_ = makeParent "a"
abbr_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
abbr_ = makeParent "abbr"
address_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
address_ = makeParent "address"
article_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
article_ = makeParent "article"
aside_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
aside_ = makeParent "aside"
audio_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
audio_ = makeParent "audio"
b_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
b_ = makeParent "b"
bdi_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
bdi_ = makeParent "bdi"
bdo_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
bdo_ = makeParent "bdo"
big_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
big_ = makeParent "big"
blockquote_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
blockquote_ = makeParent "blockquote"
body_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
body_ = makeParent "body"
button_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
button_ = makeParent "button"
canvas_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
canvas_ = makeParent "canvas"
caption_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
caption_ = makeParent "caption"
cite_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
cite_ = makeParent "cite"
code_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
code_ = makeParent "code"
colgroup_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
colgroup_ = makeParent "colgroup"
data_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
data_ = makeParent "data"
datalist_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
datalist_ = makeParent "datalist"
dd_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
dd_ = makeParent "dd"
del_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
del_ = makeParent "del"
details_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
details_ = makeParent "details"
dfn_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
dfn_ = makeParent "dfn"
div_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
div_ = makeParent "div"
dl_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
dl_ = makeParent "dl"
dt_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
dt_ = makeParent "dt"
em_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
em_ = makeParent "em"
fieldset_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
fieldset_ = makeParent "fieldset"
figcaption_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
figcaption_ = makeParent "figcaption"
figure_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
figure_ = makeParent "figure"
footer_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
footer_ = makeParent "footer"
form_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
form_ = makeParent "form"
h1_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
h1_ = makeParent "h1"
h2_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
h2_ = makeParent "h2"
h3_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
h3_ = makeParent "h3"
h4_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
h4_ = makeParent "h4"
h5_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
h5_ = makeParent "h5"
h6_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
h6_ = makeParent "h6"
head_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
head_ = makeParent "head"
header_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
header_ = makeParent "header"
html_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
html_ = makeParent "html"
i_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
i_ = makeParent "i"
iframe_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
iframe_ = makeParent "iframe"
ins_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
ins_ = makeParent "ins"
kbd_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
kbd_ = makeParent "kbd"
label_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
label_ = makeParent "label"
legend_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
legend_ = makeParent "legend"
li_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
li_ = makeParent "li"
main_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
main_ = makeParent "main"
map_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
map_ = makeParent "map"
mark_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
mark_ = makeParent "mark"
menu_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
menu_ = makeParent "menu"
menuitem_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
menuitem_ = makeParent "menuitem"
meter_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
meter_ = makeParent "meter"
nav_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
nav_ = makeParent "nav"
noscript_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
noscript_ = makeParent "noscript"
object_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
object_ = makeParent "object"
ol_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
ol_ = makeParent "ol"
optgroup_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
optgroup_ = makeParent "optgroup"
option_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
option_ = makeParent "option"
output_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
output_ = makeParent "output"
p_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
p_ = makeParent "p"
pre_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
pre_ = makeParent "pre"
progress_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
progress_ = makeParent "progress"
q_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
q_ = makeParent "q"
rp_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
rp_ = makeParent "rp"
rt_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
rt_ = makeParent "rt"
ruby_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
ruby_ = makeParent "ruby"
s_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
s_ = makeParent "signal"
samp_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
samp_ = makeParent "samp"
section_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
section_ = makeParent "section"
select_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
select_ = makeParent "select"
small_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
small_ = makeParent "small"
span_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
span_ = makeParent "span"
strong_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
strong_ = makeParent "strong"
sub_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
sub_ = makeParent "sub"
summary_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
summary_ = makeParent "summary"
sup_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
sup_ = makeParent "sup"
table_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
table_ = makeParent "table"
tbody_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
tbody_ = makeParent "tbody"
td_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
td_ = makeParent "td"
tfoot_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
tfoot_ = makeParent "tfoot"
th_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
th_ = makeParent "th"
thead_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
thead_ = makeParent "thead"
time_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
time_ = makeParent "time"
tr_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
tr_ = makeParent "tr"
u_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
u_ = makeParent "u"
ul_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
ul_ = makeParent "ul"
var_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
var_ = makeParent "var"
video_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
video_ = makeParent "video"

area_ :: Monad m => ReactT anim signal m ()
area_ = makeLeaf "area"
base_ :: Monad m => ReactT anim signal m ()
base_ = makeLeaf "base"
br_ :: Monad m => ReactT anim signal m ()
br_ = makeLeaf "br"
col_ :: Monad m => ReactT anim signal m ()
col_ = makeLeaf "col"
embed_ :: Monad m => ReactT anim signal m ()
embed_ = makeLeaf "embed"
hr_ :: Monad m => ReactT anim signal m ()
hr_ = makeLeaf "hr"
img_ :: Monad m => ReactT anim signal m ()
img_ = makeLeaf "img"
input_ :: Monad m => ReactT anim signal m ()
input_ = makeLeaf "input"
keygen_ :: Monad m => ReactT anim signal m ()
keygen_ = makeLeaf "keygen"
link_ :: Monad m => ReactT anim signal m ()
link_ = makeLeaf "link"
meta_ :: Monad m => ReactT anim signal m ()
meta_ = makeLeaf "meta"
param_ :: Monad m => ReactT anim signal m ()
param_ = makeLeaf "param"
source_ :: Monad m => ReactT anim signal m ()
source_ = makeLeaf "source"
track_ :: Monad m => ReactT anim signal m ()
track_ = makeLeaf "track"
wbr_ :: Monad m => ReactT anim signal m ()
wbr_ = makeLeaf "wbr"

-- script :: RawAttrs -> JSString -> IO ForeignNode
-- style :: RawAttrs -> JSString -> IO ForeignNode
-- textarea :: RawAttrs -> JSString -> IO ForeignNode
-- title :: RawAttrs -> JSString -> IO ForeignNode

-- svg!

svg_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
svg_ = makeParent "svg"

defs_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
defs_ = makeParent "defs"

g_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
g_ = makeParent "g"

linearGradient_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
linearGradient_ = makeParent "linearGradient"

mask_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
mask_ = makeParent "mask"

pattern_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
pattern_ = makeParent "pattern"

radialGradient_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
radialGradient_ = makeParent "radialGradient"

stop_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
stop_ = makeParent "stop"

-- text_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
-- text_ = makeParent "text"

tspan_ :: Monad m => ReactT anim signal m a -> ReactT anim signal m ()
tspan_ = makeParent "tspan"

circle_ :: Monad m => ReactT anim signal m ()
circle_ = makeLeaf "circle"

ellipse_ :: Monad m => ReactT anim signal m ()
ellipse_ = makeLeaf "ellipse"

line_ :: Monad m => ReactT anim signal m ()
line_ = makeLeaf "line"

path_ :: Monad m => ReactT anim signal m ()
path_ = makeLeaf "path"

polygon_ :: Monad m => ReactT anim signal m ()
polygon_ = makeLeaf "polygon"

polyline_ :: Monad m => ReactT anim signal m ()
polyline_ = makeLeaf "polyline"

rect_ :: Monad m => ReactT anim signal m ()
rect_ = makeLeaf "rect"
