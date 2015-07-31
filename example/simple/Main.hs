{-# LANGUAGE OverloadedStrings, NamedFieldPuns, LiberalTypeSynonyms,
    RebindableSyntax #-}
module Main where

import Prelude hiding ((>>), (=<<), return)
import Data.String

import Data.Text (Text)
import Data.Void
import React
import React.DOM
import React.GHCJS
import React.Rebindable

page_ :: Text -> ReactNode Void
page_ = classLeaf $ smartClass
    { name = "page"
    , transition = \(state, insig) -> (state + 1, Nothing)
    , initialState = 0
    , renderFn = \props state -> div_ [ class_ "parent" ] $ do
        locally $ userName_ props
        clicker_ ()
        locally $ div_ [] $ do
            clickCounter_ state
            clickCounter_ state
            clickCounter_ state
            clickCounter_ state
    }

userName_ :: Text -> ReactNode Void
userName_ = classLeaf $ dumbClass
    { name = "userName"
    , renderFn = \props _ -> div_ [ class_ "userName" ] $ do
        "User's name: "
        text_ props
    }

clicker_ :: () -> ReactNode ()
clicker_ = classLeaf $ dumbClass
    { name = "clicker"
    , renderFn = \_ _ ->
        button_ [ onClick (const (Just ())) ] "click me!"
    }

clickCounter_ :: Int -> ReactNode Void
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
    render (page_ "Joel") elem
