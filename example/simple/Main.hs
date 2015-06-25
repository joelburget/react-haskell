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
import React.DOM

page_ :: [AttrOrHandler ()] -> JSString -> ReactNode Void
page_ = classLeaf $ smartClass
    { name = "page"
    , transition = \(state, insig) -> (state + 1, Nothing)
    , initialState = 0
    , renderFn = \props state -> div_ [ class_ "parent" ] $ do
        userName_ [] props
        clicker_ [] ()
        clickCounter_ [] state
        clickCounter_ [] state
        clickCounter_ [] state
        clickCounter_ [] state
    }

userName_ :: [AttrOrHandler ()] -> JSString -> ReactNode ()
userName_ = classLeaf $ dumbClass
    { name = "userName"
    , renderFn = \props _ -> div_ [ class_ "userName" ] $ do
        "User's name: "
        text_ props
    }

clicker_ :: [AttrOrHandler ()] -> () -> ReactNode ()
clicker_ = classLeaf $ dumbClass
    { name = "clicker"
    , renderFn = \_ _ ->
        button_ [ onClick (const (Just ())) ] "click me!"
    }

clickCounter_ :: [AttrOrHandler ()] -> Int -> ReactNode ()
clickCounter_ = classLeaf $ dumbClass
    { name = "clickCounter"
    , renderFn = \props _ -> div_ [ class_ "clickCounter" ] $ do
        "Number of clicks: "
        text_ (fromString (show props))
    }

main :: IO ()
main = do
    Just doc <- currentDocument
    let elemId :: JSString
        elemId = "inject"
    Just elem <- documentGetElementById doc elemId
    render (page_ [] "Joel") elem
