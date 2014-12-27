{-# LANGUAGE OverloadedStrings, NamedFieldPuns, TypeFamilies #-}
module Simple (simpleClass) where

import Haste
import Haste.JSON
import React
import Prelude hiding (fst, snd)

-- model

data Simple
data instance PageState Simple = PageState
    { fst :: JSString
    , snd :: JSString
    , cur :: JSString -- what the user's currently typing
    }

initialState = PageState "little mac!" "pit" ""

data instance AnimationState Simple = NoAnimation

type PageState' = PageState Simple

-- update

data instance Signal Simple
    = Typing JSString
    | Enter

transition :: PageState' -> Signal Simple -> (PageState', [AnimConfig Simple])
transition state (Typing str) = (state{cur=str}, [])
transition PageState{fst, cur} Enter = (PageState cur fst "", [])

-- view

view :: PageState' -> React Simple ()
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

simpleClass :: IO (ReactClass Simple)
simpleClass = createClass view transition initialState NoAnimation []
