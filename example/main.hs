{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haste
import React
import Prelude hiding (div)

import Haste.JSON

main :: IO ()
main = do
    Just elem <- elemById "inject"
    view elem ""

view :: Elem -> JSString -> IO ()
view elem str = render elem $
    div <! className "foo" $ do
        text str

        input <! onChange (view elem . targetValue)


{-
main = do
    Just elem <- elemById "inject"
    render elem $ ReactM [] [] [
        Div [("className", Str "foo")] []
            [ Text [] [] "some string"
            , Div [("className", Str "bar")] [] []
            , Pre [] [] [ Text [] [] "this thing should be in a pre" ]
            , Text [] [] "some other string"
            ]
        ] ()
-}
