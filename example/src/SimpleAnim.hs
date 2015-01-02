{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module SimpleAnim (simpleAnimClass) where

import Haste
import Haste.JSON
import React
import Prelude hiding (fst, snd)
import Data.Map


-- model

data PageState = PageState
    { fst :: JSString
    , snd :: JSString
    , cur :: JSString
    }

type AnimState = Map JSString Double

initialState = PageState "little mac!" "pit" ""


-- update

data Transition
    = Typing JSString
    | Enter

transition :: PageState
           -> Transition
           -> (PageState, [AnimConfig Transition AnimState])
transition state (Typing str) = (state{cur=str}, [])
transition PageState{fst, cur} Enter =
    ( PageState cur fst ""
    , [AnimConfig 1000 (-20) id EaseInCubic (const Nothing)]
    )


-- view

view :: PageState -> React AnimState Transition ()
view (PageState fst snd cur) = div_ <! class_ "table" $ do
    -- animTop <- getAnimState
    let animTop = 0

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
              <! style_ (Dict [("top", Num animTop)]) $
            text_ fst

    div_ <! class_ "row" $ do
        span_ <! class_ "cell" $ " snd: "
        span_ <! class_ "relative-cell"
              <! style_ (Dict [("top", Num animTop)]) $
            text_ snd


simpleAnimClass :: IO (ReactClass PageState Transition AnimState)
simpleAnimClass = createClass view transition initialState 0 []
