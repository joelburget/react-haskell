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
view (PageState fst snd cur) = div_ <! class_ "table" $ do
    animTop <- getWithEasing EaseInCubic "Anim"
    let animTop' = -18 * animTop

    div_ <! class_ "row" $ do
        span_ <! class_ "cell" $ "next thing: "

        input_
            <! class_ "cell"
            <! value_ cur

            -- change the input value as the user types
            <! onChange (Just . Typing . targetValue)

            -- then move the user's new value to the fst and fst to snd when
            -- they enter
            <! onEnter Enter

    div_ <! class_ "row" $ do
        span_ <! class_ "cell" $ "fst: "
        span_ <! class_ "relative-cell"
              <! style_ (Dict [("top", Num animTop')]) $
            text_ fst

    div_ <! class_ "row" $ do
        span_ <! class_ "cell" $ " snd: "
        span_ <! class_ "relative-cell"
              <! style_ (Dict [("top", Num animTop')]) $
            text_ snd

main :: IO ()
main = do
    Just elem <- elemById "inject"
    cls <- createClass view transition initialState
    render elem cls initialState
    return ()
