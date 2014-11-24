{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Control.Monad.Reader
-- import Control.Monad.State

import Haste
import React
import Prelude hiding (div)

import Haste.JSON

import Data.Monoid

onEnter :: (s -> s) -> StatefulEventHandler s
onEnter f = onKeyPress handler where
    handler s KeyboardEvent{key="Enter"} = f s
    handler s _ = s

data PageState = PageState
    { _fst :: JSString
    , _snd :: JSString
    , _cur :: JSString
    }

statefulView :: StatefulReact PageState ()
statefulView = div_ $ do
    PageState fst snd cur <- getState

    input_
        <! value_ cur

        -- change the input value as the user types
        <! onChange (\state evt -> state{_cur=targetValue evt})

        -- then move the user's new value to the fst and fst to snd when
        -- they enter
        <! onEnter (\PageState{_fst=fst', _cur=cur'} ->
                    PageState cur' fst' "")

    "fst: "
    text_ fst

    " snd: "
    text_ snd

main :: IO ()
main = do
    Just elem <- elemById "inject"
    render (PageState "little mac!" "pit" "") elem statefulView

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
