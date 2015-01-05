{-# LANGUAGE OverloadedStrings, NamedFieldPuns, TypeFamilies #-}
module Simple (simpleClass) where

import Haste
import Haste.JSON
import React

-- model

data Simple
data Transition
    = Typing JSString
    | Enter
data SimpleState = SimpleState
    { fighter1 :: JSString
    , fighter2 :: JSString
    , typing :: JSString -- what the user's currently typing
    }

instance ReactKey Simple where
    type ClassState Simple = SimpleState
    type AnimationState Simple = ()
    type Signal Simple = Transition

initialState = SimpleState "little mac!" "pit" ""

-- update

transition :: SimpleState -> Transition -> (SimpleState, [AnimConfig Simple])
transition state (Typing str) = (state{typing=str}, [])
transition SimpleState{fighter1, typing} Enter =
    (SimpleState typing fighter1 "", [])

-- view

view :: SimpleState -> React Simple ()
view (SimpleState fighter1 fighter2 typing) = div_ $ do
    div_ $ do
        "send a new competitor into the ring: "
        input_
            [ value_ typing

            -- change the input value as the user types
            , onChange (Just . Typing . targetValue)

            -- then move the user's new value to the fighter1 and fighter1 to
            -- fighter2 when they enter
            , onEnter Enter
            ]

    div_ $ do
        "fighter 1: "
        text_ fighter1

    div_ $ do
        "fighter 2: "
        text_ fighter2

simpleClass :: IO (ReactClass Simple)
simpleClass = createClass view transition initialState () []
