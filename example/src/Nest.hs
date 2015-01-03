{-# LANGUAGE OverloadedStrings, NamedFieldPuns, Rank2Types, TupleSections,
  TypeFamilies, MultiParamTypeClasses #-}
module Nest where

import Data.Maybe
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

data NestingL
data instance ClassState NestingL = NestL (Maybe Int)
data instance Signal NestingL = SigL (Maybe Int)
data instance AnimationState NestingL = NoAnimationL
type ClassStateL = ClassState NestingL
type SignalL = Signal NestingL

data NestingR
data instance ClassState NestingR = NestR JSString
data instance Signal NestingR = SigR JSString
data instance AnimationState NestingR = NoAnimationR
type ClassStateR = ClassState NestingR
type SignalR = Signal NestingR

data NestingBoth
data instance ClassState NestingBoth = NestBoth (Maybe Int) JSString
data instance Signal NestingBoth = SigBL (Maybe Int) | SigBR JSString
data instance AnimationState NestingBoth = NoAnimationB
type ClassStateB = ClassState NestingBoth
type SignalB = Signal NestingBoth

instance GeneralizeClass NestingL NestingBoth where
    localizeClassState (NestBoth l _) = NestL l
    localizeAnimationState _ = NoAnimationL
    generalizeSignal (SigL l) = SigBL l

instance GeneralizeClass NestingR NestingBoth where
    localizeClassState (NestBoth _ r) = NestR r
    localizeAnimationState _ = NoAnimationR
    generalizeSignal (SigR r) = SigBR r

initialStateB :: ClassStateB
initialStateB = NestBoth (Just 1) "foo"

initialAnimationState :: AnimationState NestingBoth
initialAnimationState = NoAnimationB

-- view

transition :: ClassStateB -> SignalB -> (ClassStateB, [AnimConfig NestingBoth])
transition (NestBoth n s) (SigBL n') = (NestBoth n' s, [])
transition (NestBoth n s) (SigBR s') = (NestBoth n s', [])

generalizeLeft :: SignalL -> SignalB
generalizeLeft (SigL x) = SigBL x

generalizeRight :: SignalR -> SignalB
generalizeRight (SigR x) = SigBR x

--

pureView :: React () ()
pureView = span_ "type here: "

leftView :: Maybe Int -> React NestingL ()
leftView i = div_ $
    input_ [ value_ (iToS i)
           , onChange (Just . SigL . sToI . targetValue)
           ]

rightView :: JSString -> React NestingR ()
rightView s = div_ $
    input_ [ value_ s
           , onChange (Just . SigR . targetValue)
           ]

mainView :: ClassStateB -> React NestingBoth ()
mainView (NestBoth n s) = div_ $ do
    locally pureView
    locally (leftView n)
    locally (rightView s)

nestClass :: IO (ReactClass NestingBoth)
nestClass =
    createClass mainView transition initialStateB initialAnimationState []
