{-# LANGUAGE OverloadedStrings #-}
module React.Attrs where

import Haste.JSON
import Haste.Prim

-- class ReactAttr a where

type Attr = (JSString, JSON)

mkAttr :: JSString -> (a -> JSON) -> a -> Attr
mkAttr name f a = (name, f a)

className :: JSString -> Attr
className = mkAttr "className" Str

href :: JSString -> Attr
href = mkAttr "href" Str

id_ :: JSString -> Attr
id_ = mkAttr "id" Str

src :: JSString -> Attr
src = mkAttr "src" Str

style :: JSON -> Attr
style = mkAttr "style" id

value :: JSString -> Attr
value = mkAttr "value" Str
