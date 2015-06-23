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
    , transition = \(state, insig) -> (state + 1, Nothing)
    , initialState = 0
    , renderFn = \props state -> div_ [ class_ "parent" ] $ do
        userName_ [] props
        clicker_ [] ()
        -- button_ [ onClick (const (Just ())) ] "click me!"
        clickCounter_ [] state
        clickCounter_ [] state
        clickCounter_ [] state
        clickCounter_ [] state
    }

userName :: ReactClass String () () ()
userName = createClass $ dumbClass
    { name = "userName"
    , renderFn = \props _ -> div_ [ class_ "userName" ] $ do
        "User's name: "
        text_ props
    }

clicker :: ReactClass () () () ()
clicker = createClass $ dumbClass
    { name = "clicker"
    , renderFn = \_ _ ->
        button_ [ onClick (const (Just ())) ] "click me!"
    }

clickCounter :: ReactClass Int () () ()
clickCounter = createClass $ dumbClass
    { name = "clickCounter"
    , renderFn = \props _ -> div_ [ class_ "clickCounter" ] $ do
        "Number of clicks: "
        text_ (show props)
    }

page_ = classLeaf page
userName_ = classLeaf userName
clicker_ = classLeaf clicker
clickCounter_ = classLeaf clickCounter

main :: IO ()
main = do
    Just doc <- currentDocument
    let elemId :: JSString
        elemId = "inject"
    Just elem <- documentGetElementById doc elemId
    render (page_ [] "Joel") elem
