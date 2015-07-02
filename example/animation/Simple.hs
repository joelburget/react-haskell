{-# LANGUAGE OverloadedStrings, NamedFieldPuns, LiberalTypeSynonyms #-}
module Simple (simpleClass) where

import React
import React.DOM

-- model

data Transition
    = Typing JSString
    | Enter
data SimpleState = SimpleState
    { fighter1 :: JSString
    , fighter2 :: JSString
    , typing :: JSString -- what the user's currently typing
    }

initialState = SimpleState "little mac!" "pit" ""

-- update

transition :: Transition -> SimpleState -> (SimpleState, [AnimConfig Transition ()])
transition (Typing str) state = (state{typing=str}, [])
transition Enter SimpleState{fighter1, typing} =
    (SimpleState typing fighter1 "", [])

-- view

view :: SimpleState -> ReactNode Transition
view (SimpleState fighter1 fighter2 typing) = div_ $ do
    div_ $ do
        "send a new competitor into the ring: "
        input_
            [ value_ typing

            -- change the input value as the user types
            , onChange (Just . Typing . value . target)

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

simpleClass :: () -> ReactNode Void
simpleClass = classLeaf $ smartClass
    { name = "Simple"
    , renderFn = \_ -> view
    , initialState = initialState
    , transition = transition
    }
