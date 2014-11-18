{-# LANGUAGE OverloadedStrings #-}
module React.Elements where

-- special cases: data, main, div, span

import Haste.Prim

import React.Types

import Prelude hiding (div, span)

mkParent :: JSString -> React -> React
mkParent str (ReactM _ _ children _) = ReactM [] [] [Parent str [] [] children] ()

mkLeaf :: JSString -> React
mkLeaf str = ReactM [] [] [Leaf str [] []] ()

text :: JSString -> React
text str = ReactM [] [] [Text (fromJSStr str)] ()

a = mkParent "a"
abbr = mkParent "abbr"
address = mkParent "address"
article = mkParent "article"
aside = mkParent "aside"
audio = mkParent "audio"
b = mkParent "b"
bdi = mkParent "bdi"
bdo = mkParent "bdo"
big = mkParent "big"
blockquote = mkParent "blockquote"
body = mkParent "body"
button = mkParent "button"
canvas = mkParent "canvas"
caption = mkParent "caption"
cite = mkParent "cite"
code = mkParent "code"
colgroup = mkParent "colgroup"
data_ = mkParent "data" -- *
datalist = mkParent "datalist"
dd = mkParent "dd"
del = mkParent "del"
details = mkParent "details"
dfn = mkParent "dfn"
div = mkParent "div"
div_ = div -- *
dl = mkParent "dl"
dt = mkParent "dt"
em = mkParent "em"
fieldset = mkParent "fieldset"
figcaption = mkParent "figcaption"
figure = mkParent "figure"
footer = mkParent "footer"
form = mkParent "form"
h1 = mkParent "h1"
h2 = mkParent "h2"
h3 = mkParent "h3"
h4 = mkParent "h4"
h5 = mkParent "h5"
h6 = mkParent "h6"
head = mkParent "head"
header = mkParent "header"
html = mkParent "html"
i = mkParent "i"
iframe = mkParent "iframe"
ins = mkParent "ins"
kbd = mkParent "kbd"
label = mkParent "label"
legend = mkParent "legend"
li = mkParent "li"
main_ = mkParent "main" -- *
map = mkParent "map"
mark = mkParent "mark"
menu = mkParent "menu"
menuitem = mkParent "menuitem"
meter = mkParent "meter"
nav = mkParent "nav"
noscript = mkParent "noscript"
object = mkParent "object"
ol = mkParent "ol"
optgroup = mkParent "optgroup"
option = mkParent "option"
output = mkParent "output"
p = mkParent "p"
pre = mkParent "pre"
progress = mkParent "progress"
q = mkParent "q"
rp = mkParent "rp"
rt = mkParent "rt"
ruby = mkParent "ruby"
s = mkParent "s"
samp = mkParent "samp"
section = mkParent "section"
select = mkParent "select"
small = mkParent "small"
span = mkParent "span"
span_ = span -- *
strong = mkParent "strong"
sub = mkParent "sub"
summary = mkParent "summary"
sup = mkParent "sup"
table = mkParent "table"
tbody = mkParent "tbody"
td = mkParent "td"
tfoot = mkParent "tfoot"
th = mkParent "th"
thead = mkParent "thead"
time = mkParent "time"
tr = mkParent "tr"
u = mkParent "u"
ul = mkParent "ul"
var = mkParent "var"
video = mkParent "video"

area = mkLeaf "area"
base = mkLeaf "base"
br = mkLeaf "br"
col = mkLeaf "col"
embed = mkLeaf "embed"
hr = mkLeaf "hr"
img = mkLeaf "img"
input = mkLeaf "input"
keygen = mkLeaf "keygen"
link = mkLeaf "link"
meta = mkLeaf "meta"
param = mkLeaf "param"
source = mkLeaf "source"
track = mkLeaf "track"
wbr = mkLeaf "wbr"

-- script :: RawAttrs -> JSString -> IO ForeignNode
-- style :: RawAttrs -> JSString -> IO ForeignNode
-- textarea :: RawAttrs -> JSString -> IO ForeignNode
-- title :: RawAttrs -> JSString -> IO ForeignNode
