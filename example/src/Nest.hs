{-# LANGUAGE OverloadedStrings, NamedFieldPuns, Rank2Types, TupleSections,
  TypeFamilies, MultiParamTypeClasses, LiberalTypeSynonyms #-}
module Nest where

import Data.Maybe
import Data.Void
import Haste
import React
import Prelude hiding (fst, snd)
import Control.Applicative

import Haste.JSON
import Haste.Prim


-- utils

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
    [x] -> Just x
    _ -> Nothing

iToS :: Maybe Int -> JSString
iToS Nothing = ""
iToS (Just i) = toJSStr $ show i

sToI :: JSString -> Maybe Int
sToI = readMay . fromJSStr


-- model

type Transition = Either (Maybe Int) JSString
type State = (Maybe Int, JSString)

type NestingBoth a = a State Transition ()
type NestingL a = a (Maybe Int) (Maybe Int) ()
type NestingR a = a JSString JSString ()

instance GeneralizeSignal (Maybe Int) Transition where
    generalizeSignal = Left

instance GeneralizeSignal JSString Transition where
    generalizeSignal = Right

initialStateB :: State
initialStateB = (Just 1, "foo")

initialAnimationState :: AnimationState NestingBoth
initialAnimationState = ()

-- view

transition :: Transition -> State -> (State, [AnimConfig NestingBoth])
transition (Left n')  (n, s) = ((n', s), [])
transition (Right s') (n, s) = ((n, s'), [])

--

pureView :: Pure React'
pureView = span_ "type here: "

leftView :: Maybe Int -> NestingL React'
leftView i = div_ $
    input_ [ value_ (iToS i)
           , onChange (Just . sToI . targetValue)
           ]

rightView :: JSString -> NestingR React'
rightView s = div_ $
    input_ [ value_ s
           , onChange (Just . targetValue)
           ]

mainView :: State -> NestingBoth React'
mainView (n, s) = div_ $ do
    locally pureView
    locally (leftView n)
    locally (rightView s)

nestClass :: IO (NestingBoth ReactClass)
nestClass =
    createClass mainView transition initialStateB initialAnimationState []
