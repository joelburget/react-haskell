{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main where

import Haste
import React
import Prelude hiding (fst, snd)

import Haste.JSON

data PageState = PageState
    { fst :: JSString
    , snd :: JSString
    , cur :: JSString
    }

statefulView :: StatefulReact PageState ()
statefulView = div_ $ do
    PageState fst snd cur <- getState

    input_
        <! value_ cur

        -- change the input value as the user types
        <! onChange (\state evt -> state{cur=targetValue evt})

        -- then move the user's new value to the fst and fst to snd when
        -- they enter
        <! onEnter (\PageState{fst, cur} -> PageState cur fst "")

    "fst: "
    text_ fst

    " snd: "
    text_ snd

main :: IO ()
main = do
    Just elem <- elemById "inject"
    render (PageState "little mac!" "pit" "") elem statefulView
