{-# LANGUAGE OverloadedStrings #-}
module React.Elements where

import Haste.Prim

import React.Types

import Prelude hiding (div, span)

makeParent :: Monad m
           => JSString
           -> ReactT ty m a
           -> ReactT ty m ()
makeParent name children = ReactT $ \anim -> do
    ~(childNodes, _) <- runReactT children anim
    return ([Parent name [] [] childNodes], ())

makeLeaf :: Monad m
         => JSString
         -> ReactT ty m ()
makeLeaf name = ReactT $ \_ -> return ([Leaf name [] []], ())

text_ :: JSString -> React ty ()
text_ str = ReactT $ \_ -> return ([Text (fromJSStr str)], ())

a_ :: Monad m => ReactT ty m a -> ReactT ty m ()
a_ = makeParent "a"
abbr_ :: Monad m => ReactT ty m a -> ReactT ty m ()
abbr_ = makeParent "abbr"
address_ :: Monad m => ReactT ty m a -> ReactT ty m ()
address_ = makeParent "address"
article_ :: Monad m => ReactT ty m a -> ReactT ty m ()
article_ = makeParent "article"
aside_ :: Monad m => ReactT ty m a -> ReactT ty m ()
aside_ = makeParent "aside"
audio_ :: Monad m => ReactT ty m a -> ReactT ty m ()
audio_ = makeParent "audio"
b_ :: Monad m => ReactT ty m a -> ReactT ty m ()
b_ = makeParent "b"
bdi_ :: Monad m => ReactT ty m a -> ReactT ty m ()
bdi_ = makeParent "bdi"
bdo_ :: Monad m => ReactT ty m a -> ReactT ty m ()
bdo_ = makeParent "bdo"
big_ :: Monad m => ReactT ty m a -> ReactT ty m ()
big_ = makeParent "big"
blockquote_ :: Monad m => ReactT ty m a -> ReactT ty m ()
blockquote_ = makeParent "blockquote"
body_ :: Monad m => ReactT ty m a -> ReactT ty m ()
body_ = makeParent "body"
button_ :: Monad m => ReactT ty m a -> ReactT ty m ()
button_ = makeParent "button"
canvas_ :: Monad m => ReactT ty m a -> ReactT ty m ()
canvas_ = makeParent "canvas"
caption_ :: Monad m => ReactT ty m a -> ReactT ty m ()
caption_ = makeParent "caption"
cite_ :: Monad m => ReactT ty m a -> ReactT ty m ()
cite_ = makeParent "cite"
code_ :: Monad m => ReactT ty m a -> ReactT ty m ()
code_ = makeParent "code"
colgroup_ :: Monad m => ReactT ty m a -> ReactT ty m ()
colgroup_ = makeParent "colgroup"
data_ :: Monad m => ReactT ty m a -> ReactT ty m ()
data_ = makeParent "data"
datalist_ :: Monad m => ReactT ty m a -> ReactT ty m ()
datalist_ = makeParent "datalist"
dd_ :: Monad m => ReactT ty m a -> ReactT ty m ()
dd_ = makeParent "dd"
del_ :: Monad m => ReactT ty m a -> ReactT ty m ()
del_ = makeParent "del"
details_ :: Monad m => ReactT ty m a -> ReactT ty m ()
details_ = makeParent "details"
dfn_ :: Monad m => ReactT ty m a -> ReactT ty m ()
dfn_ = makeParent "dfn"
div_ :: Monad m => ReactT ty m a -> ReactT ty m ()
div_ = makeParent "div"
dl_ :: Monad m => ReactT ty m a -> ReactT ty m ()
dl_ = makeParent "dl"
dt_ :: Monad m => ReactT ty m a -> ReactT ty m ()
dt_ = makeParent "dt"
em_ :: Monad m => ReactT ty m a -> ReactT ty m ()
em_ = makeParent "em"
fieldset_ :: Monad m => ReactT ty m a -> ReactT ty m ()
fieldset_ = makeParent "fieldset"
figcaption_ :: Monad m => ReactT ty m a -> ReactT ty m ()
figcaption_ = makeParent "figcaption"
figure_ :: Monad m => ReactT ty m a -> ReactT ty m ()
figure_ = makeParent "figure"
footer_ :: Monad m => ReactT ty m a -> ReactT ty m ()
footer_ = makeParent "footer"
form_ :: Monad m => ReactT ty m a -> ReactT ty m ()
form_ = makeParent "form"
h1_ :: Monad m => ReactT ty m a -> ReactT ty m ()
h1_ = makeParent "h1"
h2_ :: Monad m => ReactT ty m a -> ReactT ty m ()
h2_ = makeParent "h2"
h3_ :: Monad m => ReactT ty m a -> ReactT ty m ()
h3_ = makeParent "h3"
h4_ :: Monad m => ReactT ty m a -> ReactT ty m ()
h4_ = makeParent "h4"
h5_ :: Monad m => ReactT ty m a -> ReactT ty m ()
h5_ = makeParent "h5"
h6_ :: Monad m => ReactT ty m a -> ReactT ty m ()
h6_ = makeParent "h6"
head_ :: Monad m => ReactT ty m a -> ReactT ty m ()
head_ = makeParent "head"
header_ :: Monad m => ReactT ty m a -> ReactT ty m ()
header_ = makeParent "header"
html_ :: Monad m => ReactT ty m a -> ReactT ty m ()
html_ = makeParent "html"
i_ :: Monad m => ReactT ty m a -> ReactT ty m ()
i_ = makeParent "i"
iframe_ :: Monad m => ReactT ty m a -> ReactT ty m ()
iframe_ = makeParent "iframe"
ins_ :: Monad m => ReactT ty m a -> ReactT ty m ()
ins_ = makeParent "ins"
kbd_ :: Monad m => ReactT ty m a -> ReactT ty m ()
kbd_ = makeParent "kbd"
label_ :: Monad m => ReactT ty m a -> ReactT ty m ()
label_ = makeParent "label"
legend_ :: Monad m => ReactT ty m a -> ReactT ty m ()
legend_ = makeParent "legend"
li_ :: Monad m => ReactT ty m a -> ReactT ty m ()
li_ = makeParent "li"
main_ :: Monad m => ReactT ty m a -> ReactT ty m ()
main_ = makeParent "main"
map_ :: Monad m => ReactT ty m a -> ReactT ty m ()
map_ = makeParent "map"
mark_ :: Monad m => ReactT ty m a -> ReactT ty m ()
mark_ = makeParent "mark"
menu_ :: Monad m => ReactT ty m a -> ReactT ty m ()
menu_ = makeParent "menu"
menuitem_ :: Monad m => ReactT ty m a -> ReactT ty m ()
menuitem_ = makeParent "menuitem"
meter_ :: Monad m => ReactT ty m a -> ReactT ty m ()
meter_ = makeParent "meter"
nav_ :: Monad m => ReactT ty m a -> ReactT ty m ()
nav_ = makeParent "nav"
noscript_ :: Monad m => ReactT ty m a -> ReactT ty m ()
noscript_ = makeParent "noscript"
object_ :: Monad m => ReactT ty m a -> ReactT ty m ()
object_ = makeParent "object"
ol_ :: Monad m => ReactT ty m a -> ReactT ty m ()
ol_ = makeParent "ol"
optgroup_ :: Monad m => ReactT ty m a -> ReactT ty m ()
optgroup_ = makeParent "optgroup"
option_ :: Monad m => ReactT ty m a -> ReactT ty m ()
option_ = makeParent "option"
output_ :: Monad m => ReactT ty m a -> ReactT ty m ()
output_ = makeParent "output"
p_ :: Monad m => ReactT ty m a -> ReactT ty m ()
p_ = makeParent "p"
pre_ :: Monad m => ReactT ty m a -> ReactT ty m ()
pre_ = makeParent "pre"
progress_ :: Monad m => ReactT ty m a -> ReactT ty m ()
progress_ = makeParent "progress"
q_ :: Monad m => ReactT ty m a -> ReactT ty m ()
q_ = makeParent "q"
rp_ :: Monad m => ReactT ty m a -> ReactT ty m ()
rp_ = makeParent "rp"
rt_ :: Monad m => ReactT ty m a -> ReactT ty m ()
rt_ = makeParent "rt"
ruby_ :: Monad m => ReactT ty m a -> ReactT ty m ()
ruby_ = makeParent "ruby"
s_ :: Monad m => ReactT ty m a -> ReactT ty m ()
s_ = makeParent "signal"
samp_ :: Monad m => ReactT ty m a -> ReactT ty m ()
samp_ = makeParent "samp"
section_ :: Monad m => ReactT ty m a -> ReactT ty m ()
section_ = makeParent "section"
select_ :: Monad m => ReactT ty m a -> ReactT ty m ()
select_ = makeParent "select"
small_ :: Monad m => ReactT ty m a -> ReactT ty m ()
small_ = makeParent "small"
span_ :: Monad m => ReactT ty m a -> ReactT ty m ()
span_ = makeParent "span"
strong_ :: Monad m => ReactT ty m a -> ReactT ty m ()
strong_ = makeParent "strong"
sub_ :: Monad m => ReactT ty m a -> ReactT ty m ()
sub_ = makeParent "sub"
summary_ :: Monad m => ReactT ty m a -> ReactT ty m ()
summary_ = makeParent "summary"
sup_ :: Monad m => ReactT ty m a -> ReactT ty m ()
sup_ = makeParent "sup"
table_ :: Monad m => ReactT ty m a -> ReactT ty m ()
table_ = makeParent "table"
tbody_ :: Monad m => ReactT ty m a -> ReactT ty m ()
tbody_ = makeParent "tbody"
td_ :: Monad m => ReactT ty m a -> ReactT ty m ()
td_ = makeParent "td"
tfoot_ :: Monad m => ReactT ty m a -> ReactT ty m ()
tfoot_ = makeParent "tfoot"
th_ :: Monad m => ReactT ty m a -> ReactT ty m ()
th_ = makeParent "th"
thead_ :: Monad m => ReactT ty m a -> ReactT ty m ()
thead_ = makeParent "thead"
time_ :: Monad m => ReactT ty m a -> ReactT ty m ()
time_ = makeParent "time"
tr_ :: Monad m => ReactT ty m a -> ReactT ty m ()
tr_ = makeParent "tr"
u_ :: Monad m => ReactT ty m a -> ReactT ty m ()
u_ = makeParent "u"
ul_ :: Monad m => ReactT ty m a -> ReactT ty m ()
ul_ = makeParent "ul"
var_ :: Monad m => ReactT ty m a -> ReactT ty m ()
var_ = makeParent "var"
video_ :: Monad m => ReactT ty m a -> ReactT ty m ()
video_ = makeParent "video"

area_ :: Monad m => ReactT ty m ()
area_ = makeLeaf "area"
base_ :: Monad m => ReactT ty m ()
base_ = makeLeaf "base"
br_ :: Monad m => ReactT ty m ()
br_ = makeLeaf "br"
col_ :: Monad m => ReactT ty m ()
col_ = makeLeaf "col"
embed_ :: Monad m => ReactT ty m ()
embed_ = makeLeaf "embed"
hr_ :: Monad m => ReactT ty m ()
hr_ = makeLeaf "hr"
img_ :: Monad m => ReactT ty m ()
img_ = makeLeaf "img"
input_ :: Monad m => ReactT ty m ()
input_ = makeLeaf "input"
keygen_ :: Monad m => ReactT ty m ()
keygen_ = makeLeaf "keygen"
link_ :: Monad m => ReactT ty m ()
link_ = makeLeaf "link"
meta_ :: Monad m => ReactT ty m ()
meta_ = makeLeaf "meta"
param_ :: Monad m => ReactT ty m ()
param_ = makeLeaf "param"
source_ :: Monad m => ReactT ty m ()
source_ = makeLeaf "source"
track_ :: Monad m => ReactT ty m ()
track_ = makeLeaf "track"
wbr_ :: Monad m => ReactT ty m ()
wbr_ = makeLeaf "wbr"

-- script :: RawAttrs -> JSString -> IO ForeignNode
-- style :: RawAttrs -> JSString -> IO ForeignNode
-- textarea :: RawAttrs -> JSString -> IO ForeignNode
-- title :: RawAttrs -> JSString -> IO ForeignNode

-- svg!

svg_ :: Monad m => ReactT ty m a -> ReactT ty m ()
svg_ = makeParent "svg"

defs_ :: Monad m => ReactT ty m a -> ReactT ty m ()
defs_ = makeParent "defs"

g_ :: Monad m => ReactT ty m a -> ReactT ty m ()
g_ = makeParent "g"

linearGradient_ :: Monad m => ReactT ty m a -> ReactT ty m ()
linearGradient_ = makeParent "linearGradient"

mask_ :: Monad m => ReactT ty m a -> ReactT ty m ()
mask_ = makeParent "mask"

pattern_ :: Monad m => ReactT ty m a -> ReactT ty m ()
pattern_ = makeParent "pattern"

radialGradient_ :: Monad m => ReactT ty m a -> ReactT ty m ()
radialGradient_ = makeParent "radialGradient"

stop_ :: Monad m => ReactT ty m a -> ReactT ty m ()
stop_ = makeParent "stop"

-- text_ :: Monad m => ReactT ty m a -> ReactT ty m ()
-- text_ = makeParent "text"

tspan_ :: Monad m => ReactT ty m a -> ReactT ty m ()
tspan_ = makeParent "tspan"

circle_ :: Monad m => ReactT ty m ()
circle_ = makeLeaf "circle"

ellipse_ :: Monad m => ReactT ty m ()
ellipse_ = makeLeaf "ellipse"

line_ :: Monad m => ReactT ty m ()
line_ = makeLeaf "line"

path_ :: Monad m => ReactT ty m ()
path_ = makeLeaf "path"

polygon_ :: Monad m => ReactT ty m ()
polygon_ = makeLeaf "polygon"

polyline_ :: Monad m => ReactT ty m ()
polyline_ = makeLeaf "polyline"

rect_ :: Monad m => ReactT ty m ()
rect_ = makeLeaf "rect"
