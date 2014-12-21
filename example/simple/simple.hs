{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main where

import Haste
import React
import Prelude hiding (fst, snd)

import Haste.JSON

-- model

data PageState = PageState
    { fst :: JSString
    , snd :: JSString
    , cur :: JSString
    }

initialState = PageState "little mac!" "pit" ""

-- update

data Transition
    = Typing JSString
    | Enter

transition :: PageState -> Transition -> PageState
transition state (Typing str) = state{cur=str}
transition PageState{fst, cur} Enter = PageState cur fst ""

-- view

view :: PageState -> React Transition ()
view (PageState fst snd cur) = div_ $ do
    input_
        <! value_ cur

        -- change the input value as the user types
        <! onChange (Just . Typing . targetValue)

        -- then move the user's new value to the fst and fst to snd when
        -- they enter
        <! onEnter Enter

    "fst: "
    text_ fst

    " snd: "
    text_ snd

main :: IO ()
main = do
    Just elem <- elemById "inject"
    render elem view transition initialState
