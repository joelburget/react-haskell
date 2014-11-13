{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haste
import React
import Prelude hiding (div)

import Haste.JSON

main = do
    Just elem <- elemById "inject"
    renderComponent elem $ div <! className "foo" $ do
        "some string"

        div <! className "bar" $ return ()

        pre "this thing should be in a pre"

        "some other string"

{-
main = do
    Just elem <- elemById "inject"
    renderComponent elem $ ReactM [] [] [
        Div [("className", Str "foo")] []
            [ Text [] [] "some string"
            , Div [("className", Str "bar")] [] []
            , Pre [] [] [ Text [] [] "this thing should be in a pre" ]
            , Text [] [] "some other string"
            ]
        ] ()
-}
