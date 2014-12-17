{-# LANGUAGE OverloadedStrings #-}
module React.Elements where

import Haste.Prim

import React.Types

import Prelude hiding (div, span)

makeParent :: Monad m
           => JSString
           -> ReactT s m a
           -> ReactT s m ()
makeParent name children = ReactT $ do
    ~(childNodes, _) <- runReactT children
    return ([Parent name [] [] childNodes], ())

makeLeaf :: Monad m
         => JSString
         -> ReactT s m ()
makeLeaf name = ReactT $ return ([Leaf name [] []], ())

text_ :: JSString -> React s ()
text_ str = ReactT $ return ([Text (fromJSStr str)], ())

a_ :: Monad m => ReactT s m a -> ReactT s m ()
a_ = makeParent "a"
abbr_ :: Monad m => ReactT s m a -> ReactT s m ()
abbr_ = makeParent "abbr"
address_ :: Monad m => ReactT s m a -> ReactT s m ()
address_ = makeParent "address"
article_ :: Monad m => ReactT s m a -> ReactT s m ()
article_ = makeParent "article"
aside_ :: Monad m => ReactT s m a -> ReactT s m ()
aside_ = makeParent "aside"
audio_ :: Monad m => ReactT s m a -> ReactT s m ()
audio_ = makeParent "audio"
b_ :: Monad m => ReactT s m a -> ReactT s m ()
b_ = makeParent "b"
bdi_ :: Monad m => ReactT s m a -> ReactT s m ()
bdi_ = makeParent "bdi"
bdo_ :: Monad m => ReactT s m a -> ReactT s m ()
bdo_ = makeParent "bdo"
big_ :: Monad m => ReactT s m a -> ReactT s m ()
big_ = makeParent "big"
blockquote_ :: Monad m => ReactT s m a -> ReactT s m ()
blockquote_ = makeParent "blockquote"
body_ :: Monad m => ReactT s m a -> ReactT s m ()
body_ = makeParent "body"
button_ :: Monad m => ReactT s m a -> ReactT s m ()
button_ = makeParent "button"
canvas_ :: Monad m => ReactT s m a -> ReactT s m ()
canvas_ = makeParent "canvas"
caption_ :: Monad m => ReactT s m a -> ReactT s m ()
caption_ = makeParent "caption"
cite_ :: Monad m => ReactT s m a -> ReactT s m ()
cite_ = makeParent "cite"
code_ :: Monad m => ReactT s m a -> ReactT s m ()
code_ = makeParent "code"
colgroup_ :: Monad m => ReactT s m a -> ReactT s m ()
colgroup_ = makeParent "colgroup"
data_ :: Monad m => ReactT s m a -> ReactT s m ()
data_ = makeParent "data"
datalist_ :: Monad m => ReactT s m a -> ReactT s m ()
datalist_ = makeParent "datalist"
dd_ :: Monad m => ReactT s m a -> ReactT s m ()
dd_ = makeParent "dd"
del_ :: Monad m => ReactT s m a -> ReactT s m ()
del_ = makeParent "del"
details_ :: Monad m => ReactT s m a -> ReactT s m ()
details_ = makeParent "details"
dfn_ :: Monad m => ReactT s m a -> ReactT s m ()
dfn_ = makeParent "dfn"
div_ :: Monad m => ReactT s m a -> ReactT s m ()
div_ = makeParent "div"
dl_ :: Monad m => ReactT s m a -> ReactT s m ()
dl_ = makeParent "dl"
dt_ :: Monad m => ReactT s m a -> ReactT s m ()
dt_ = makeParent "dt"
em_ :: Monad m => ReactT s m a -> ReactT s m ()
em_ = makeParent "em"
fieldset_ :: Monad m => ReactT s m a -> ReactT s m ()
fieldset_ = makeParent "fieldset"
figcaption_ :: Monad m => ReactT s m a -> ReactT s m ()
figcaption_ = makeParent "figcaption"
figure_ :: Monad m => ReactT s m a -> ReactT s m ()
figure_ = makeParent "figure"
footer_ :: Monad m => ReactT s m a -> ReactT s m ()
footer_ = makeParent "footer"
form_ :: Monad m => ReactT s m a -> ReactT s m ()
form_ = makeParent "form"
h1_ :: Monad m => ReactT s m a -> ReactT s m ()
h1_ = makeParent "h1"
h2_ :: Monad m => ReactT s m a -> ReactT s m ()
h2_ = makeParent "h2"
h3_ :: Monad m => ReactT s m a -> ReactT s m ()
h3_ = makeParent "h3"
h4_ :: Monad m => ReactT s m a -> ReactT s m ()
h4_ = makeParent "h4"
h5_ :: Monad m => ReactT s m a -> ReactT s m ()
h5_ = makeParent "h5"
h6_ :: Monad m => ReactT s m a -> ReactT s m ()
h6_ = makeParent "h6"
head_ :: Monad m => ReactT s m a -> ReactT s m ()
head_ = makeParent "head"
header_ :: Monad m => ReactT s m a -> ReactT s m ()
header_ = makeParent "header"
html_ :: Monad m => ReactT s m a -> ReactT s m ()
html_ = makeParent "html"
i_ :: Monad m => ReactT s m a -> ReactT s m ()
i_ = makeParent "i"
iframe_ :: Monad m => ReactT s m a -> ReactT s m ()
iframe_ = makeParent "iframe"
ins_ :: Monad m => ReactT s m a -> ReactT s m ()
ins_ = makeParent "ins"
kbd_ :: Monad m => ReactT s m a -> ReactT s m ()
kbd_ = makeParent "kbd"
label_ :: Monad m => ReactT s m a -> ReactT s m ()
label_ = makeParent "label"
legend_ :: Monad m => ReactT s m a -> ReactT s m ()
legend_ = makeParent "legend"
li_ :: Monad m => ReactT s m a -> ReactT s m ()
li_ = makeParent "li"
main_ :: Monad m => ReactT s m a -> ReactT s m ()
main_ = makeParent "main"
map_ :: Monad m => ReactT s m a -> ReactT s m ()
map_ = makeParent "map"
mark_ :: Monad m => ReactT s m a -> ReactT s m ()
mark_ = makeParent "mark"
menu_ :: Monad m => ReactT s m a -> ReactT s m ()
menu_ = makeParent "menu"
menuitem_ :: Monad m => ReactT s m a -> ReactT s m ()
menuitem_ = makeParent "menuitem"
meter_ :: Monad m => ReactT s m a -> ReactT s m ()
meter_ = makeParent "meter"
nav_ :: Monad m => ReactT s m a -> ReactT s m ()
nav_ = makeParent "nav"
noscript_ :: Monad m => ReactT s m a -> ReactT s m ()
noscript_ = makeParent "noscript"
object_ :: Monad m => ReactT s m a -> ReactT s m ()
object_ = makeParent "object"
ol_ :: Monad m => ReactT s m a -> ReactT s m ()
ol_ = makeParent "ol"
optgroup_ :: Monad m => ReactT s m a -> ReactT s m ()
optgroup_ = makeParent "optgroup"
option_ :: Monad m => ReactT s m a -> ReactT s m ()
option_ = makeParent "option"
output_ :: Monad m => ReactT s m a -> ReactT s m ()
output_ = makeParent "output"
p_ :: Monad m => ReactT s m a -> ReactT s m ()
p_ = makeParent "p"
pre_ :: Monad m => ReactT s m a -> ReactT s m ()
pre_ = makeParent "pre"
progress_ :: Monad m => ReactT s m a -> ReactT s m ()
progress_ = makeParent "progress"
q_ :: Monad m => ReactT s m a -> ReactT s m ()
q_ = makeParent "q"
rp_ :: Monad m => ReactT s m a -> ReactT s m ()
rp_ = makeParent "rp"
rt_ :: Monad m => ReactT s m a -> ReactT s m ()
rt_ = makeParent "rt"
ruby_ :: Monad m => ReactT s m a -> ReactT s m ()
ruby_ = makeParent "ruby"
s_ :: Monad m => ReactT s m a -> ReactT s m ()
s_ = makeParent "s"
samp_ :: Monad m => ReactT s m a -> ReactT s m ()
samp_ = makeParent "samp"
section_ :: Monad m => ReactT s m a -> ReactT s m ()
section_ = makeParent "section"
select_ :: Monad m => ReactT s m a -> ReactT s m ()
select_ = makeParent "select"
small_ :: Monad m => ReactT s m a -> ReactT s m ()
small_ = makeParent "small"
span_ :: Monad m => ReactT s m a -> ReactT s m ()
span_ = makeParent "span"
strong_ :: Monad m => ReactT s m a -> ReactT s m ()
strong_ = makeParent "strong"
sub_ :: Monad m => ReactT s m a -> ReactT s m ()
sub_ = makeParent "sub"
summary_ :: Monad m => ReactT s m a -> ReactT s m ()
summary_ = makeParent "summary"
sup_ :: Monad m => ReactT s m a -> ReactT s m ()
sup_ = makeParent "sup"
table_ :: Monad m => ReactT s m a -> ReactT s m ()
table_ = makeParent "table"
tbody_ :: Monad m => ReactT s m a -> ReactT s m ()
tbody_ = makeParent "tbody"
td_ :: Monad m => ReactT s m a -> ReactT s m ()
td_ = makeParent "td"
tfoot_ :: Monad m => ReactT s m a -> ReactT s m ()
tfoot_ = makeParent "tfoot"
th_ :: Monad m => ReactT s m a -> ReactT s m ()
th_ = makeParent "th"
thead_ :: Monad m => ReactT s m a -> ReactT s m ()
thead_ = makeParent "thead"
time_ :: Monad m => ReactT s m a -> ReactT s m ()
time_ = makeParent "time"
tr_ :: Monad m => ReactT s m a -> ReactT s m ()
tr_ = makeParent "tr"
u_ :: Monad m => ReactT s m a -> ReactT s m ()
u_ = makeParent "u"
ul_ :: Monad m => ReactT s m a -> ReactT s m ()
ul_ = makeParent "ul"
var_ :: Monad m => ReactT s m a -> ReactT s m ()
var_ = makeParent "var"
video_ :: Monad m => ReactT s m a -> ReactT s m ()
video_ = makeParent "video"

area_ :: Monad m => ReactT s m ()
area_ = makeLeaf "area"
base_ :: Monad m => ReactT s m ()
base_ = makeLeaf "base"
br_ :: Monad m => ReactT s m ()
br_ = makeLeaf "br"
col_ :: Monad m => ReactT s m ()
col_ = makeLeaf "col"
embed_ :: Monad m => ReactT s m ()
embed_ = makeLeaf "embed"
hr_ :: Monad m => ReactT s m ()
hr_ = makeLeaf "hr"
img_ :: Monad m => ReactT s m ()
img_ = makeLeaf "img"
input_ :: Monad m => ReactT s m ()
input_ = makeLeaf "input"
keygen_ :: Monad m => ReactT s m ()
keygen_ = makeLeaf "keygen"
link_ :: Monad m => ReactT s m ()
link_ = makeLeaf "link"
meta_ :: Monad m => ReactT s m ()
meta_ = makeLeaf "meta"
param_ :: Monad m => ReactT s m ()
param_ = makeLeaf "param"
source_ :: Monad m => ReactT s m ()
source_ = makeLeaf "source"
track_ :: Monad m => ReactT s m ()
track_ = makeLeaf "track"
wbr_ :: Monad m => ReactT s m ()
wbr_ = makeLeaf "wbr"

-- script :: RawAttrs -> JSString -> IO ForeignNode
-- style :: RawAttrs -> JSString -> IO ForeignNode
-- textarea :: RawAttrs -> JSString -> IO ForeignNode
-- title :: RawAttrs -> JSString -> IO ForeignNode
