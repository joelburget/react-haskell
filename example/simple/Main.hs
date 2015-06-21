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

page :: ReactClass Int String String Void
page = createClass $ smartClass
    { name = "page"
    , transition = \(state, insig) -> (state, undefined)
    , initialState = "this is state!"
    , renderFn = page2
    }

page2 :: Int -> String -> ReactNode String
page2 props state = div_ [ class_ "parent" ] $ do
    span_ [ class_ "hooray", onClick (const (Just "clicked!")) ] "spanish"
    header' [] state
    footer' [] props

header :: ReactClass String () String String
header = createClass $ dumbClass
    { name = "header"
    , renderFn = \props _ -> div_ [ class_ "header" ] $ do
        "header: "
        text_ props
    }

footer :: ReactClass Int () String String
footer = createClass $ dumbClass
    { name = "name"
    , renderFn = \props _ -> div_ [ class_ "footer" ] $ do
        "footer: "
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
    -- debugRender page2 elem
    render (page' [] 5) elem
