{-# LANGUAGE OverloadedStrings, NamedFieldPuns, LiberalTypeSynonyms #-}
module Simple (simpleClass) where

import Control.Applicative

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import React
import React.Class

-- model

data Transition
    = Typing JSString
    | Enter

data SimpleState = SimpleState
    { fighter1 :: JSString
    , fighter2 :: JSString
    , typing :: JSString -- what the user's currently typing
    }

-- ambiguous IsString / ToJSString :(
f1Key, f2Key, tKey :: JSString
f1Key = "fighter1"
f2Key = "fighter2"
tKey = "typing"

instance ToJSRef SimpleState where
    toJSRef (SimpleState f1 f2 t) = do
        obj <- newObj
        setProp f1Key f1 obj
        setProp f2Key f2 obj
        setProp tKey t obj
        return obj

instance FromJSRef SimpleState where
    fromJSRef obj = do
        f1 <- getPropMaybe f1Key obj
        f2 <- getPropMaybe f2Key obj
        t  <- getPropMaybe tKey obj
        return $ SimpleState <$> f1 <*> f2 <*> t

type Simple a = a SimpleState Transition

initialState = SimpleState "little mac!" "pit" ""

-- update

transition :: Transition -> SimpleState -> SimpleState
transition (Typing str) state = state{typing=str}
transition Enter SimpleState{fighter1, typing} =
    SimpleState typing fighter1 ""

-- view

view :: SimpleState -> Simple React'
view (SimpleState fighter1 fighter2 typing) = div_ $ do
    div_ $ do
        "send a new competitor into the ring: "
        div_ [] $ text_ typing
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

simpleClass :: IO (Simple ReactClass)
simpleClass = createClass view transition initialState []
