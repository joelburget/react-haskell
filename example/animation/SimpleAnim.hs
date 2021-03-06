{-# LANGUAGE OverloadedStrings, NamedFieldPuns, LiberalTypeSynonyms #-}
module SimpleAnim (simpleAnimClass) where

import React
import Prelude hiding (fst, snd)
import Data.Map


-- model

data ClassState = ClassState
    { fst :: JSString
    , snd :: JSString
    , cur :: JSString
    }

data Transition
    = Typing JSString
    | Enter

type AnimationState = Map JSString Double

type SimpleAnim a = a ClassState Transition AnimationState

initialState = ClassState "little mac!" "pit" ""


-- update

transition :: Transition
           -> ClassState
           -> (ClassState, [AnimConfig Transition AnimationState])
transition (Typing str) state = (state{cur=str}, [])
transition Enter ClassState{fst, cur} =
    ( ClassState cur fst ""
    , [AnimConfig 1000 (-20, 0) id EaseInCubic (const Nothing)]
    )


-- view

view :: ClassState -> SimpleAnim React'
view (ClassState fst snd cur) = div_ [ class_ "table" ] $ do
    -- animTop <- getAnimationState
    let animTop = 0

    div_ [ class_ "row" ] $ do
        span_ [ class_ "cell" ] $ "next thing: "

        input_
            [ class_ "cell"
            , value_ cur

            -- change the input value as the user types
            , onChange (Just . Typing . value . target)

            -- then move the user's new value to the fst and fst to snd when
            -- they enter
            , onEnter Enter
            ]

    div_ [ class_ "row" ] $ do
        span_ [ class_ "cell" ] "fst: "
        span_ [ class_ "relative-cell"
              , style_ (Dict [("top", Num animTop)])
              ] $
            text_ fst

    div_ [ class_ "row" ] $ do
        span_ [ class_ "cell" ] $ " snd: "
        span_ [ class_ "relative-cell"
              , style_ (Dict [("top", Num animTop)])
              ] $
            text_ snd


simpleAnimClass :: IO (SimpleAnim ReactClass)
simpleAnimClass = createClass view transition initialState 0 []
