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
    pageLayout :: ImportedClass NoProps Side

data Side = LeftSide | RightSide

transitionHandler :: ((Int, Int), Side) -> ((Int, Int), Maybe Void)
transitionHandler ((l, r), LeftSide)  = ((l + 1, r), Nothing)
transitionHandler ((l, r), RightSide) = ((l, r + 1), Nothing)

page_ :: () -> ReactNode Void
page_ = classLeaf $ smartClass
    { name = "page"
    , transition = transitionHandler
    , initialState = (0, 0)
    , renderFn = \_ (left, right) -> pageLayout_ $ do
            clicker_ LeftSide
            counter_ left

            clicker_ RightSide
            counter_ right
    }

pageLayout_ :: ReactNode Side -> ReactNode Side
pageLayout_ = importParentClass pageLayout noProps

clicker_ :: Side -> ReactNode Side
clicker_ = classLeaf $ dumbClass
    { name = "clicker"
    , renderFn = \side _ ->
        button_ [ onClick (const (Just side)) ] "click me!"
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
