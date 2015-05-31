{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleInstances, DataKinds #-}
module React.Elements where

import GHCJS.Foreign
import GHCJS.Types

import React.Imports
import React.Types


-- | Parent nodes always take children, but can also optionally take a list
-- of arguments.
--
-- Example of the first case, which exercises the simpler instance:
--
-- @
-- div_ $ ... children ...
-- @
--
-- Example of the second, which exercises the more complicated instance:
--
-- @
-- span_ [class_ "example"] $ ... children ...
-- @
class TermParent result where
    -- | The argument to a parent term is either:
    --
    -- * a list of attributes (@[AttrOrHandler (Signal sig)]@), which leads
    --   to a result type of @ReactT ty state sig m a -> ReactT ty state sig m
    --   a@.
    --
    -- * or children (@ReactT ty state sig m a@), which leads to a result type
    -- of @ReactT ty state sig m a@.
    type TermParentArg result :: *

    termParent :: ForeignRender -> TermParentArg result -> result


instance (f ~ ReactT ty state sig) =>
        TermParent (f -> ReactT RtBuiltin state sig) where
    type TermParentArg (f -> ReactT RtBuiltin state sig) = [AttrOrHandler sig]

    -- TODO questionable whether foreign nodes should use ReactTBuiltin. Maybe
    -- create a ReactTForeign?
    termParent render attrs children =
        let (hs, as) = separateAttrs attrs
            childNodes = runReactT children
        in ReactTBuiltin [Static (Parent render as hs childNodes)]


instance TermParent (ReactT RtBuiltin state sig) where
    type TermParentArg (ReactT RtBuiltin state sig) =
        ReactT RtBuiltin state sig

    -- TODO questionable whether foreign nodes should use ReactTBuiltin. Maybe
    -- create a ReactTForeign?
    termParent render children = ReactTBuiltin $
        [Static (Parent render [] [] (runReactT children))]


foreignParent :: TermParent t
              => ForeignRender
              -> TermParentArg t
              -> t
foreignParent = termParent


reactParent :: TermParent t
            => JSString
            -> TermParentArg t
            -> t
reactParent name = termParent (js_React_DOM_parent name)


termLeaf :: ForeignRender
         -> [AttrOrHandler sig]
         -> ReactT RtBuiltin state sig
-- TODO questionable whether foreign nodes should use ReactTBuiltin. Maybe
-- create a ReactTForeign?
termLeaf render attrs =
    let (hs, as) = separateAttrs attrs
    in ReactTBuiltin [Static (Leaf render as hs)]


foreignLeaf :: ForeignRender
            -> [AttrOrHandler sig]
            -> ReactT RtBuiltin state sig
foreignLeaf = termLeaf


reactLeaf :: JSString
         -> [AttrOrHandler sig]
         -> ReactT RtBuiltin state sig
reactLeaf name = termLeaf (\as' _ -> js_React_DOM_leaf name as')


-- TODO ToJSString a => ?
-- Would this just be annoyingly ambiguous?
text_ :: JSString -> ReactT RtBuiltin state sig
text_ str = ReactTBuiltin $ [Static $ Text (fromJSString str)]

-- TODO generate these automatically
a_ :: TermParent t => TermParentArg t -> t
a_ = reactParent "a"

abbr_ :: TermParent t => TermParentArg t -> t
abbr_ = reactParent "abbr"

address_ :: TermParent t => TermParentArg t -> t
address_ = reactParent "address"

article_ :: TermParent t => TermParentArg t -> t
article_ = reactParent "article"

aside_ :: TermParent t => TermParentArg t -> t
aside_ = reactParent "aside"

audio_ :: TermParent t => TermParentArg t -> t
audio_ = reactParent "audio"

b_ :: TermParent t => TermParentArg t -> t
b_ = reactParent "b"

bdi_ :: TermParent t => TermParentArg t -> t
bdi_ = reactParent "bdi"

bdo_ :: TermParent t => TermParentArg t -> t
bdo_ = reactParent "bdo"

big_ :: TermParent t => TermParentArg t -> t
big_ = reactParent "big"

blockquote_ :: TermParent t => TermParentArg t -> t
blockquote_ = reactParent "blockquote"

body_ :: TermParent t => TermParentArg t -> t
body_ = reactParent "body"

button_ :: TermParent t => TermParentArg t -> t
button_ = reactParent "button"

canvas_ :: TermParent t => TermParentArg t -> t
canvas_ = reactParent "canvas"

caption_ :: TermParent t => TermParentArg t -> t
caption_ = reactParent "caption"

cite_ :: TermParent t => TermParentArg t -> t
cite_ = reactParent "cite"

code_ :: TermParent t => TermParentArg t -> t
code_ = reactParent "code"

colgroup_ :: TermParent t => TermParentArg t -> t
colgroup_ = reactParent "colgroup"

data_ :: TermParent t => TermParentArg t -> t
data_ = reactParent "data"

datalist_ :: TermParent t => TermParentArg t -> t
datalist_ = reactParent "datalist"

dd_ :: TermParent t => TermParentArg t -> t
dd_ = reactParent "dd"

del_ :: TermParent t => TermParentArg t -> t
del_ = reactParent "del"

details_ :: TermParent t => TermParentArg t -> t
details_ = reactParent "details"

dfn_ :: TermParent t => TermParentArg t -> t
dfn_ = reactParent "dfn"

div_ :: TermParent t => TermParentArg t -> t
div_ = reactParent "div"

dl_ :: TermParent t => TermParentArg t -> t
dl_ = reactParent "dl"

dt_ :: TermParent t => TermParentArg t -> t
dt_ = reactParent "dt"

em_ :: TermParent t => TermParentArg t -> t
em_ = reactParent "em"

fieldset_ :: TermParent t => TermParentArg t -> t
fieldset_ = reactParent "fieldset"

figcaption_ :: TermParent t => TermParentArg t -> t
figcaption_ = reactParent "figcaption"

figure_ :: TermParent t => TermParentArg t -> t
figure_ = reactParent "figure"

footer_ :: TermParent t => TermParentArg t -> t
footer_ = reactParent "footer"

form_ :: TermParent t => TermParentArg t -> t
form_ = reactParent "form"

h1_ :: TermParent t => TermParentArg t -> t
h1_ = reactParent "h1"

h2_ :: TermParent t => TermParentArg t -> t
h2_ = reactParent "h2"

h3_ :: TermParent t => TermParentArg t -> t
h3_ = reactParent "h3"

h4_ :: TermParent t => TermParentArg t -> t
h4_ = reactParent "h4"

h5_ :: TermParent t => TermParentArg t -> t
h5_ = reactParent "h5"

h6_ :: TermParent t => TermParentArg t -> t
h6_ = reactParent "h6"

head_ :: TermParent t => TermParentArg t -> t
head_ = reactParent "head"

header_ :: TermParent t => TermParentArg t -> t
header_ = reactParent "header"

html_ :: TermParent t => TermParentArg t -> t
html_ = reactParent "html"

i_ :: TermParent t => TermParentArg t -> t
i_ = reactParent "i"

iframe_ :: TermParent t => TermParentArg t -> t
iframe_ = reactParent "iframe"

ins_ :: TermParent t => TermParentArg t -> t
ins_ = reactParent "ins"

kbd_ :: TermParent t => TermParentArg t -> t
kbd_ = reactParent "kbd"

label_ :: TermParent t => TermParentArg t -> t
label_ = reactParent "label"

legend_ :: TermParent t => TermParentArg t -> t
legend_ = reactParent "legend"

li_ :: TermParent t => TermParentArg t -> t
li_ = reactParent "li"

main_ :: TermParent t => TermParentArg t -> t
main_ = reactParent "main"

map_ :: TermParent t => TermParentArg t -> t
map_ = reactParent "map"

mark_ :: TermParent t => TermParentArg t -> t
mark_ = reactParent "mark"

menu_ :: TermParent t => TermParentArg t -> t
menu_ = reactParent "menu"

menuitem_ :: TermParent t => TermParentArg t -> t
menuitem_ = reactParent "menuitem"

meter_ :: TermParent t => TermParentArg t -> t
meter_ = reactParent "meter"

nav_ :: TermParent t => TermParentArg t -> t
nav_ = reactParent "nav"

noscript_ :: TermParent t => TermParentArg t -> t
noscript_ = reactParent "noscript"

object_ :: TermParent t => TermParentArg t -> t
object_ = reactParent "object"

ol_ :: TermParent t => TermParentArg t -> t
ol_ = reactParent "ol"

optgroup_ :: TermParent t => TermParentArg t -> t
optgroup_ = reactParent "optgroup"

option_ :: TermParent t => TermParentArg t -> t
option_ = reactParent "option"

output_ :: TermParent t => TermParentArg t -> t
output_ = reactParent "output"

p_ :: TermParent t => TermParentArg t -> t
p_ = reactParent "p"

pre_ :: TermParent t => TermParentArg t -> t
pre_ = reactParent "pre"

progress_ :: TermParent t => TermParentArg t -> t
progress_ = reactParent "progress"

q_ :: TermParent t => TermParentArg t -> t
q_ = reactParent "q"

rp_ :: TermParent t => TermParentArg t -> t
rp_ = reactParent "rp"

rt_ :: TermParent t => TermParentArg t -> t
rt_ = reactParent "rt"

ruby_ :: TermParent t => TermParentArg t -> t
ruby_ = reactParent "ruby"

s_ :: TermParent t => TermParentArg t -> t
s_ = reactParent "signal"

samp_ :: TermParent t => TermParentArg t -> t
samp_ = reactParent "samp"

section_ :: TermParent t => TermParentArg t -> t
section_ = reactParent "section"

select_ :: TermParent t => TermParentArg t -> t
select_ = reactParent "select"

small_ :: TermParent t => TermParentArg t -> t
small_ = reactParent "small"

span_ :: TermParent t => TermParentArg t -> t
span_ = reactParent "span"

strong_ :: TermParent t => TermParentArg t -> t
strong_ = reactParent "strong"

sub_ :: TermParent t => TermParentArg t -> t
sub_ = reactParent "sub"

summary_ :: TermParent t => TermParentArg t -> t
summary_ = reactParent "summary"

sup_ :: TermParent t => TermParentArg t -> t
sup_ = reactParent "sup"

table_ :: TermParent t => TermParentArg t -> t
table_ = reactParent "table"

tbody_ :: TermParent t => TermParentArg t -> t
tbody_ = reactParent "tbody"

td_ :: TermParent t => TermParentArg t -> t
td_ = reactParent "td"

tfoot_ :: TermParent t => TermParentArg t -> t
tfoot_ = reactParent "tfoot"

th_ :: TermParent t => TermParentArg t -> t
th_ = reactParent "th"

thead_ :: TermParent t => TermParentArg t -> t
thead_ = reactParent "thead"

time_ :: TermParent t => TermParentArg t -> t
time_ = reactParent "time"

tr_ :: TermParent t => TermParentArg t -> t
tr_ = reactParent "tr"

u_ :: TermParent t => TermParentArg t -> t
u_ = reactParent "u"

ul_ :: TermParent t => TermParentArg t -> t
ul_ = reactParent "ul"

var_ :: TermParent t => TermParentArg t -> t
var_ = reactParent "var"

video_ :: TermParent t => TermParentArg t -> t
video_ = reactParent "video"


area_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
area_ = reactLeaf "area"

base_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
base_ = reactLeaf "base"

br_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
br_ = reactLeaf "br"

col_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
col_ = reactLeaf "col"

embed_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
embed_ = reactLeaf "embed"

hr_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
hr_ = reactLeaf "hr"

img_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
img_ = reactLeaf "img"

input_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
input_ = reactLeaf "input"

keygen_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
keygen_ = reactLeaf "keygen"

link_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
link_ = reactLeaf "link"

meta_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
meta_ = reactLeaf "meta"

param_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
param_ = reactLeaf "param"

source_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
source_ = reactLeaf "source"

track_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
track_ = reactLeaf "track"

wbr_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
wbr_ = reactLeaf "wbr"

-- script :: RawAttrs -> JSString -> IO ForeignNode
-- style :: RawAttrs -> JSString -> IO ForeignNode
-- textarea :: RawAttrs -> JSString -> IO ForeignNode
-- title :: RawAttrs -> JSString -> IO ForeignNode

-- svg!

svg_ :: TermParent t => TermParentArg t -> t
svg_ = reactParent "svg"

defs_ :: TermParent t => TermParentArg t -> t
defs_ = reactParent "defs"

g_ :: TermParent t => TermParentArg t -> t
g_ = reactParent "g"

linearGradient_ :: TermParent t => TermParentArg t -> t
linearGradient_ = reactParent "linearGradient"

mask_ :: TermParent t => TermParentArg t -> t
mask_ = reactParent "mask"

pattern_ :: TermParent t => TermParentArg t -> t
pattern_ = reactParent "pattern"

radialGradient_ :: TermParent t => TermParentArg t -> t
radialGradient_ = reactParent "radialGradient"

stop_ :: TermParent t => TermParentArg t -> t
stop_ = reactParent "stop"

-- text_ :: TermParent t => TermParentArg t -> t
-- text_ = reactParent "text"

tspan_ :: TermParent t => TermParentArg t -> t
tspan_ = reactParent "tspan"

circle_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
circle_ = reactLeaf "circle"

ellipse_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
ellipse_ = reactLeaf "ellipse"

line_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
line_ = reactLeaf "line"

path_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
path_ = reactLeaf "path"

polygon_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
polygon_ = reactLeaf "polygon"

polyline_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
polyline_ = reactLeaf "polyline"

rect_ :: [AttrOrHandler sig] -> ReactT RtBuiltin state sig
rect_ = reactLeaf "rect"
