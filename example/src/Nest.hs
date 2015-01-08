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
instance ReactKey NestingBoth where
    type ClassState NestingBoth = State
    type Signal NestingBoth = Transition
    type AnimationState NestingBoth = ()

data NestingL
instance ReactKey NestingL where
    type ClassState NestingL = Maybe Int
    type Signal NestingL = Maybe Int
    type AnimationState NestingL = ()

data NestingR
instance ReactKey NestingR where
    type ClassState NestingR = JSString
    type Signal NestingR = JSString
    type AnimationState NestingR = ()

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

transition :: Transition -> State -> (State, [AnimConfig NestingBoth])
transition (Left n')  (n, s) = ((n', s), [])
transition (Right s') (n, s) = ((n, s'), [])

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
