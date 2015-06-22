{-# LANGUAGE OverloadedStrings, NamedFieldPuns, LiberalTypeSynonyms,
    RebindableSyntax #-}
module Main where

import Prelude hiding ((>>), (=<<), return)
import Data.String

import Data.Void
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Types (Document)
import GHCJS.DOM.Document (documentGetElementById)
import React

page :: ReactClass String Int () Void
page = createClass $ smartClass
    { name = "page"
    , transition = \(state, insig) -> (state + 1, undefined)
    , initialState = 0
    , renderFn = \props state -> div_ [ class_ "parent" ] $ do
        header' [] props
        button_ [ onClick (const (Just ())) ] "click me!"
        footer' [] state
    }

header :: ReactClass String () () ()
header = createClass $ dumbClass
    { name = "header"
    , renderFn = \props _ -> div_ [ class_ "header" ] $ do
        "User's name: "
        text_ props
    }

footer :: ReactClass Int () () ()
footer = createClass $ dumbClass
    { name = "name"
    , renderFn = \props _ -> div_ [ class_ "footer" ] $ do
        "Number of clicks: "
        text_ (show props)
    }

page' = classLeaf page
header' = classLeaf header
footer' = classLeaf footer

main :: IO ()
main = do
    Just doc <- currentDocument
    let elemId :: JSString
        elemId = "inject"
    Just elem <- documentGetElementById doc elemId
    render (page' [] "Joel") elem
