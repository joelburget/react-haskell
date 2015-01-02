{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Simple (simpleClass) where

import Haste
import Haste.JSON
import React
import Prelude hiding (fst, snd)

-- model

data PageState = PageState
    { fst :: JSString
    , snd :: JSString
    , cur :: JSString -- what the user's currently typing
    }

initialState = PageState "little mac!" "pit" ""

-- update

data Transition
    = Typing JSString
    | Enter

transition :: PageState -> Transition -> (PageState, [AnimConfig Transition ()])
transition state (Typing str) = (state{cur=str}, [])
transition PageState{fst, cur} Enter = (PageState cur fst "", [])

-- view

view :: PageState -> React () Transition ()
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

simpleClass :: IO (ReactClass PageState Transition ())
simpleClass = createClass view transition initialState () []
