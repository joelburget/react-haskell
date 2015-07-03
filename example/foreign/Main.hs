{-# LANGUAGE OverloadedStrings, NamedFieldPuns, LiberalTypeSynonyms,
    RebindableSyntax, JavaScriptFFI #-}
module Main where

import Prelude hiding ((>>), (=<<), return)
import Control.Applicative
import Data.String
import Data.Void

import React
import React.DOM
import React.GHCJS
import React.Rebindable

foreign import javascript "window.Layout"
    pageLayout :: ImportedClass ()

page_ :: () -> ReactNode Void
page_ = classLeaf $ smartClass
    { name = "page"
    , transition = \(state, insig) -> (state + 1, Nothing)
    , initialState = 0
    , renderFn = \_ state -> pageLayout_ $ do
            clicker_ ()
            counter_ state
    }

pageLayout_ :: ReactNode () -> ReactNode ()
pageLayout_ = importParentClass pageLayout

clicker_ :: () -> ReactNode ()
clicker_ = classLeaf $ dumbClass
    { name = "clicker"
    , renderFn = \_ _ ->
        button_ [ onClick (const (Just ())) ] "click me!"
    }

counter_ :: Int -> ReactNode a
counter_ = classLeaf $ dumbClass
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
    render (page_ ()) elem
