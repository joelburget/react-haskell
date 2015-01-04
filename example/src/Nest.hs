{-# LANGUAGE OverloadedStrings, NamedFieldPuns, Rank2Types, TupleSections,
  TypeFamilies, MultiParamTypeClasses #-}
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

data NestingBoth
type Transition = Either (Maybe Int) JSString
type State = (Maybe Int, JSString)
type instance ClassState NestingBoth = State
type instance Signal NestingBoth = Transition
type instance AnimationState NestingBoth = ()

data NestingL
type instance ClassState NestingL = Maybe Int
type instance Signal NestingL = Maybe Int
type instance AnimationState NestingL = ()

data NestingR
type instance ClassState NestingR = JSString
type instance Signal NestingR = JSString
type instance AnimationState NestingR = ()

narrowL :: Narrowing NestingBoth NestingL
narrowL = Narrowing id Left

narrowR :: Narrowing NestingBoth NestingR
narrowR = Narrowing id Right

narrowPure :: Narrowing NestingBoth ()
narrowPure = Narrowing (const ()) absurd

initialStateB :: (Maybe Int, JSString)
initialStateB = (Just 1, "foo")

initialAnimationState :: AnimationState NestingBoth
initialAnimationState = ()

-- view

transition :: State -> Transition -> (State, [AnimConfig NestingBoth])
transition (n, s) (Left n') = ((n', s), [])
transition (n, s) (Right s') = ((n, s'), [])

--

pureView :: React () ()
pureView = span_ "type here: "

leftView :: Maybe Int -> React NestingL ()
leftView i = div_ $
    input_ [ value_ (iToS i)
           , onChange (Just . sToI . targetValue)
           ]

rightView :: JSString -> React NestingR ()
rightView s = div_ $
    input_ [ value_ s
           , onChange (Just . targetValue)
           ]

mainView :: State -> React NestingBoth ()
mainView (n, s) = div_ $ do
    locally narrowPure pureView
    locally narrowL (leftView n)
    locally narrowR (rightView s)

nestClass :: IO (ReactClass NestingBoth)
nestClass =
    createClass mainView transition initialStateB initialAnimationState []
