{-# LANGUAGE OverloadedStrings, NamedFieldPuns, TypeFamilies #-}
module Simple (simpleClass) where

import Haste
import Haste.JSON
import React
import Prelude hiding (fst, snd)

-- model

data Simple
data Transition
    = Typing JSString
    | Enter
data SimpleState = SimpleState
    { fst :: JSString
    , snd :: JSString
    , cur :: JSString -- what the user's currently typing
    }

instance ReactKey Simple where
    type ClassState Simple = SimpleState
    type AnimationState Simple = ()
    type Signal Simple = Transition

initialState = SimpleState "little mac!" "pit" ""

-- update

transition :: SimpleState -> Transition -> (SimpleState, [AnimConfig Simple])
transition state (Typing str) = (state{cur=str}, [])
transition SimpleState{fst, cur} Enter = (SimpleState cur fst "", [])

-- view

view :: SimpleState -> React Simple ()
view (SimpleState fst snd cur) = div_ $ do
    input_
        [ value_ cur

        -- change the input value as the user types
        , onChange (Just . Typing . targetValue)

        -- then move the user's new value to the fst and fst to snd when
        -- they enter
        , onEnter Enter
        ]

    "fst: "
    text_ fst

    " snd: "
    text_ snd

simpleClass :: IO (ReactClass Simple)
simpleClass = createClass view transition initialState () []
