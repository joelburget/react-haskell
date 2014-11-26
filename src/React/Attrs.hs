{-# LANGUAGE OverloadedStrings #-}
module React.Attrs where

import Haste.JSON
import Haste.Prim

-- class ReactAttr a where

type Attr = (JSString, JSON)

mkAttr :: JSString -> (a -> JSON) -> a -> Attr
mkAttr name f a = (name, f a)

-- exception
className :: JSString -> Attr
className = mkAttr "className" Str

class_ :: JSString -> Attr
class_ = mkAttr "className" Str

href_ :: JSString -> Attr
href_ = mkAttr "href" Str

id_ :: JSString -> Attr
id_ = mkAttr "id" Str

src_ :: JSString -> Attr
src_ = mkAttr "src" Str

style_ :: JSON -> Attr
style_ = mkAttr "style" id

value_ :: JSString -> Attr
value_ = mkAttr "value" Str

placeholder_ :: JSString -> Attr
placeholder_ = mkAttr "placeholder" Str

for_ :: JSString -> Attr
for_ = mkAttr "htmlFor" Str

type_ :: JSString -> Attr
type_ = mkAttr "type" Str

checked_ :: Bool -> Attr
checked_ = mkAttr "checked" Bool

-- TODO think about supporting autoFocus / other camel-cased react names
autofocus_ :: Bool -> Attr
autofocus_ = mkAttr "autoFocus" Bool
