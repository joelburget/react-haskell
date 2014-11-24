{-# LANGUAGE OverloadedStrings #-}
module React.Elements where

import Haste.Prim

import React.Types

import Prelude hiding (div, span)

makeParent :: Monad m
           => JSString
           -> StatefulReactT s m a
           -> StatefulReactT s m ()
makeParent name children = StatefulReactT $ \s -> do
    ~(childNodes, s', _) <- runStatefulReactT children s
    return ([Parent name [] [] childNodes], s', ())

makeLeaf :: Monad m
         => JSString
         -> StatefulReactT s m ()
makeLeaf name = StatefulReactT $ \s -> return ([Leaf name [] []], s, ())

text_ :: JSString -> StatefulReact s ()
text_ str = StatefulReactT $ \s -> return ([Text (fromJSStr str)], s, ())

a_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
a_ = makeParent "a"
abbr_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
abbr_ = makeParent "abbr"
address_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
address_ = makeParent "address"
article_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
article_ = makeParent "article"
aside_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
aside_ = makeParent "aside"
audio_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
audio_ = makeParent "audio"
b_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
b_ = makeParent "b"
bdi_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
bdi_ = makeParent "bdi"
bdo_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
bdo_ = makeParent "bdo"
big_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
big_ = makeParent "big"
blockquote_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
blockquote_ = makeParent "blockquote"
body_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
body_ = makeParent "body"
button_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
button_ = makeParent "button"
canvas_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
canvas_ = makeParent "canvas"
caption_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
caption_ = makeParent "caption"
cite_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
cite_ = makeParent "cite"
code_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
code_ = makeParent "code"
colgroup_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
colgroup_ = makeParent "colgroup"
data_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
data_ = makeParent "data"
datalist_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
datalist_ = makeParent "datalist"
dd_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
dd_ = makeParent "dd"
del_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
del_ = makeParent "del"
details_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
details_ = makeParent "details"
dfn_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
dfn_ = makeParent "dfn"
div_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
div_ = makeParent "div"
dl_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
dl_ = makeParent "dl"
dt_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
dt_ = makeParent "dt"
em_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
em_ = makeParent "em"
fieldset_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
fieldset_ = makeParent "fieldset"
figcaption_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
figcaption_ = makeParent "figcaption"
figure_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
figure_ = makeParent "figure"
footer_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
footer_ = makeParent "footer"
form_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
form_ = makeParent "form"
h1_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
h1_ = makeParent "h1"
h2_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
h2_ = makeParent "h2"
h3_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
h3_ = makeParent "h3"
h4_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
h4_ = makeParent "h4"
h5_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
h5_ = makeParent "h5"
h6_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
h6_ = makeParent "h6"
head_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
head_ = makeParent "head"
header_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
header_ = makeParent "header"
html_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
html_ = makeParent "html"
i_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
i_ = makeParent "i"
iframe_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
iframe_ = makeParent "iframe"
ins_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
ins_ = makeParent "ins"
kbd_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
kbd_ = makeParent "kbd"
label_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
label_ = makeParent "label"
legend_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
legend_ = makeParent "legend"
li_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
li_ = makeParent "li"
main_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
main_ = makeParent "main"
map_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
map_ = makeParent "map"
mark_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
mark_ = makeParent "mark"
menu_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
menu_ = makeParent "menu"
menuitem_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
menuitem_ = makeParent "menuitem"
meter_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
meter_ = makeParent "meter"
nav_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
nav_ = makeParent "nav"
noscript_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
noscript_ = makeParent "noscript"
object_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
object_ = makeParent "object"
ol_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
ol_ = makeParent "ol"
optgroup_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
optgroup_ = makeParent "optgroup"
option_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
option_ = makeParent "option"
output_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
output_ = makeParent "output"
p_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
p_ = makeParent "p"
pre_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
pre_ = makeParent "pre"
progress_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
progress_ = makeParent "progress"
q_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
q_ = makeParent "q"
rp_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
rp_ = makeParent "rp"
rt_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
rt_ = makeParent "rt"
ruby_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
ruby_ = makeParent "ruby"
s_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
s_ = makeParent "s"
samp_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
samp_ = makeParent "samp"
section_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
section_ = makeParent "section"
select_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
select_ = makeParent "select"
small_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
small_ = makeParent "small"
span_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
span_ = makeParent "span"
strong_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
strong_ = makeParent "strong"
sub_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
sub_ = makeParent "sub"
summary_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
summary_ = makeParent "summary"
sup_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
sup_ = makeParent "sup"
table_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
table_ = makeParent "table"
tbody_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
tbody_ = makeParent "tbody"
td_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
td_ = makeParent "td"
tfoot_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
tfoot_ = makeParent "tfoot"
th_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
th_ = makeParent "th"
thead_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
thead_ = makeParent "thead"
time_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
time_ = makeParent "time"
tr_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
tr_ = makeParent "tr"
u_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
u_ = makeParent "u"
ul_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
ul_ = makeParent "ul"
var_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
var_ = makeParent "var"
video_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
video_ = makeParent "video"

area_ :: Monad m => StatefulReactT s m ()
area_ = makeLeaf "area"
base_ :: Monad m => StatefulReactT s m ()
base_ = makeLeaf "base"
br_ :: Monad m => StatefulReactT s m ()
br_ = makeLeaf "br"
col_ :: Monad m => StatefulReactT s m ()
col_ = makeLeaf "col"
embed_ :: Monad m => StatefulReactT s m ()
embed_ = makeLeaf "embed"
hr_ :: Monad m => StatefulReactT s m ()
hr_ = makeLeaf "hr"
img_ :: Monad m => StatefulReactT s m ()
img_ = makeLeaf "img"
input_ :: Monad m => StatefulReactT s m ()
input_ = makeLeaf "input"
keygen_ :: Monad m => StatefulReactT s m ()
keygen_ = makeLeaf "keygen"
link_ :: Monad m => StatefulReactT s m ()
link_ = makeLeaf "link"
meta_ :: Monad m => StatefulReactT s m ()
meta_ = makeLeaf "meta"
param_ :: Monad m => StatefulReactT s m ()
param_ = makeLeaf "param"
source_ :: Monad m => StatefulReactT s m ()
source_ = makeLeaf "source"
track_ :: Monad m => StatefulReactT s m ()
track_ = makeLeaf "track"
wbr_ :: Monad m => StatefulReactT s m ()
wbr_ = makeLeaf "wbr"

-- script :: RawAttrs -> JSString -> IO ForeignNode
-- style :: RawAttrs -> JSString -> IO ForeignNode
-- textarea :: RawAttrs -> JSString -> IO ForeignNode
-- title :: RawAttrs -> JSString -> IO ForeignNode
