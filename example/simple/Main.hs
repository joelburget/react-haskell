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

page :: ReactClass () () String Void
page = createClass $ smartClass
    { name = "page"
    , transition = \(state, insig) -> (state, undefined)
    , getInitialState = ()
    , renderFn = \_ _ -> page2
    }

page' = classLeaf page

page2 :: ReactNode String
page2 = div_ [ class_ "parent" ] $ do
    span_ [ class_ "hooray", onClick (const (Just "clicked!")) ] "spanish"
    "hello world!"

main :: IO ()
main = do
    Just doc <- currentDocument
    let elemId :: JSString
        elemId = "inject"
    Just elem <- documentGetElementById doc elemId
    -- debugRender page2 elem
    render (page' [] ()) elem
