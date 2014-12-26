{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main where

import Haste
import Haste.JSON
import React
import Prelude hiding (fst, snd)

-- model

data PageState = PageState
    { fst :: JSString
    , snd :: JSString
    , cur :: JSString
    }

type AnimationState = Double

initialState = PageState "little mac!" "pit" ""

-- update

data Transition
    = Typing JSString
    | Enter

transition :: PageState
           -> Transition
           -> (PageState, Maybe (AnimConfig Transition))
transition state (Typing str) = (state{cur=str}, Nothing)
transition PageState{fst, cur} Enter =
    ( PageState cur fst ""
    , Just (AnimConfig 1000 "Anim" {-(-18) EaseInCubic-} (const Nothing))
    )

-- view

view :: PageState -> React AnimationState Transition ()
view (PageState fst snd cur) = div_ <! style_ (Dict [("display", "table")]) $ do
    animTop <- getWithEasing EaseInCubic "Anim"
    let animTop' = -18 * animTop

    div_ <! style_ (Dict [("display", "table-row")]) $ do
        span_ <! style_ (Dict [("display", "table-cell")]) $ "next thing: "

        input_
            <! style_ (Dict [("display", "table-cell")])
            <! value_ cur

            -- change the input value as the user types
            <! onChange (Just . Typing . targetValue)

            -- then move the user's new value to the fst and fst to snd when
            -- they enter
            <! onEnter Enter

    div_ <! style_ (Dict [("display", "table-row")]) $ do
        span_ <! style_ (Dict [("display", "table-row")]) $ "fst: "
        span_ <! style_ (Dict [("position", "relative"),
                               ("display", "table-cell"),
                               ("top", Num animTop')]) $
            text_ fst

    div_ <! style_ (Dict [("display", "table-row")]) $ do
        span_ <! style_ (Dict [("display", "table-row")]) $ " snd: "
        span_ <! style_ (Dict [("position", "relative"),
                               ("display", "table-cell"),
                               ("top", Num animTop')]) $
            text_ snd

main :: IO ()
main = do
    Just elem <- elemById "inject"
    cls <- createClass view transition initialState
    render elem cls initialState
    return ()
