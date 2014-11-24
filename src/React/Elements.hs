{-# LANGUAGE OverloadedStrings #-}
module React.Elements where

-- TODO: un-special-case
-- special cases: data, main, div, span

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

-- input_ :: StatefulReact s ()
-- div_ :: StatefulReact s a -> StatefulReact s ()

a_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
a_ = makeParent "a"
-- abbr_ = makeParent "abbr"
-- address_ = makeParent "address"
-- article_ = makeParent "article"
-- aside_ = makeParent "aside"
-- audio_ = makeParent "audio"
-- b_ = makeParent "b"
-- bdi_ = makeParent "bdi"
-- bdo_ = makeParent "bdo"
-- big_ = makeParent "big"
-- blockquote_ = makeParent "blockquote"
-- body_ = makeParent "body"
-- button_ = makeParent "button"
-- canvas_ = makeParent "canvas"
-- caption_ = makeParent "caption"
-- cite_ = makeParent "cite"
-- code_ = makeParent "code"
-- colgroup_ = makeParent "colgroup"
-- data_ = makeParent "data"
-- datalist_ = makeParent "datalist"
-- dd_ = makeParent "dd"
-- del_ = makeParent "del"
-- details_ = makeParent "details"
-- dfn_ = makeParent "dfn"
div_ :: Monad m => StatefulReactT s m a -> StatefulReactT s m ()
div_ = makeParent "div"
-- dl_ = makeParent "dl"
-- dt_ = makeParent "dt"
-- em_ = makeParent "em"
-- fieldset_ = makeParent "fieldset"
-- figcaption_ = makeParent "figcaption"
-- figure_ = makeParent "figure"
-- footer_ = makeParent "footer"
-- form_ = makeParent "form"
-- h1_ = makeParent "h1"
-- h2_ = makeParent "h2"
-- h3_ = makeParent "h3"
-- h4_ = makeParent "h4"
-- h5_ = makeParent "h5"
-- h6_ = makeParent "h6"
-- head_ = makeParent "head"
-- header_ = makeParent "header"
-- html_ = makeParent "html"
-- i_ = makeParent "i"
-- iframe_ = makeParent "iframe"
-- ins_ = makeParent "ins"
-- kbd_ = makeParent "kbd"
-- label_ = makeParent "label"
-- legend_ = makeParent "legend"
-- li_ = makeParent "li"
-- main_ = makeParent "main"
-- map_ = makeParent "map"
-- mark_ = makeParent "mark"
-- menu_ = makeParent "menu"
-- menuitem_ = makeParent "menuitem"
-- meter_ = makeParent "meter"
-- nav_ = makeParent "nav"
-- noscript_ = makeParent "noscript"
-- object_ = makeParent "object"
-- ol_ = makeParent "ol"
-- optgroup_ = makeParent "optgroup"
-- option_ = makeParent "option"
-- output_ = makeParent "output"
-- p_ = makeParent "p"
-- pre_ = makeParent "pre"
-- progress_ = makeParent "progress"
-- q_ = makeParent "q"
-- rp_ = makeParent "rp"
-- rt_ = makeParent "rt"
-- ruby_ = makeParent "ruby"
-- s_ = makeParent "s"
-- samp_ = makeParent "samp"
-- section_ = makeParent "section"
-- select_ = makeParent "select"
-- small_ = makeParent "small"
-- span_ = makeParent "span"
-- strong_ = makeParent "strong"
-- sub_ = makeParent "sub"
-- summary_ = makeParent "summary"
-- sup_ = makeParent "sup"
-- table_ = makeParent "table"
-- tbody_ = makeParent "tbody"
-- td_ = makeParent "td"
-- tfoot_ = makeParent "tfoot"
-- th_ = makeParent "th"
-- thead_ = makeParent "thead"
-- time_ = makeParent "time"
-- tr_ = makeParent "tr"
-- u_ = makeParent "u"
-- ul_ = makeParent "ul"
-- var_ = makeParent "var"
-- video_ = makeParent "video"

area_ :: Monad m => StatefulReactT s m ()
area_ = makeLeaf "area"

base_ :: Monad m => StatefulReactT s m ()
base_ = makeLeaf "base"
-- br_ = makeLeaf "br"
-- col_ = makeLeaf "col"
-- embed_ = makeLeaf "embed"
-- hr_ = makeLeaf "hr"
-- img_ = makeLeaf "img"
input_ :: Monad m => StatefulReactT s m ()
input_ = makeLeaf "input"
-- keygen_ = makeLeaf "keygen"
-- link_ = makeLeaf "link"
-- meta_ = makeLeaf "meta"
-- param_ = makeLeaf "param"
-- source_ = makeLeaf "source"
-- track_ = makeLeaf "track"
-- wbr_ = makeLeaf "wbr"

-- script :: RawAttrs -> JSString -> IO ForeignNode
-- style :: RawAttrs -> JSString -> IO ForeignNode
-- textarea :: RawAttrs -> JSString -> IO ForeignNode
-- title :: RawAttrs -> JSString -> IO ForeignNode
