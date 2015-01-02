{-# LANGUAGE OverloadedStrings, TypeFamilies, Rank2Types #-}
module Circles (circlesClass) where

import Control.Applicative
import Control.Monad
import Data.String (fromString)

import Lens.Family2

import Haste hiding (fromString)
import Haste.JSON
import React

-- model

data Circ = C1 | C2 | C3 | C4 deriving (Eq, Show)
data PageState = PageState Circ [Circ]
data AnimationState = AnimationState Color Color Color Color (Double, Double)


coord :: Circ -> (Double, Double)
coord C1 = (-1, -1)
coord C2 = (1, -1)
coord C3 = (1, 1)
coord C4 = (-1, 1)


circOrder :: [Circ]
circOrder = cycle [C1, C3, C2, C4]


initialState :: PageState
initialState = PageState C1 (tail circOrder)


initialAnimationState :: AnimationState
initialAnimationState =
    AnimationState animZero animZero animZero animZero (0, 0)


colorL :: Circ -> Lens' AnimationState Color
colorL C1 f (AnimationState c1 c2 c3 c4 pt) =
    (\x -> AnimationState x c2 c3 c4 pt) <$> f c1
colorL C2 f (AnimationState c1 c2 c3 c4 pt) =
    (\x -> AnimationState c1 x c3 c4 pt) <$> f c2
colorL C3 f (AnimationState c1 c2 c3 c4 pt) =
    (\x -> AnimationState c1 c2 x c4 pt) <$> f c3
colorL C4 f (AnimationState c1 c2 c3 c4 pt) =
    (\x -> AnimationState c1 c2 c3 x pt) <$> f c4


ptL :: Lens' AnimationState (Double, Double)
ptL f (AnimationState c1 c2 c3 c4 pt) = AnimationState c1 c2 c3 c4 <$> f pt


-- update

data Transition = Flash

transition :: PageState
           -> Transition
           -> (PageState, [AnimConfig Transition AnimationState])
transition (PageState c' (c:cs)) Flash =
    let coordTrans = coord c `animSub` coord c'
        colorTrans = fillorange `animSub` fillblue
    in ( PageState c cs
       , [ AnimConfig 800 colorTrans (colorL c) EaseInQuad (const (Just Flash))
         , AnimConfig 2000 coordTrans ptL EaseInOutQuad (const Nothing)
         ]
       )


-- view

fillblue, fillorange :: Color
fillblue = Color 85 161 220
fillorange = Color 245 175 51


fill_' = fill_ . fromString . show


-- PureReact ()
circ :: (Double, Double) -> Color -> React AnimationState Transition ()
circ (x, y) color = circle_ <! cx_ x
                            <! cy_ y
                            <! r_ 0.15
                            <! fill_' color


mainView :: PageState -> React AnimationState Transition ()
mainView (PageState c _) = div_ $ do
    AnimationState c1 c2 c3 c4 trans <- getAnimState

    svg_ <! width_ 800
         <! height_ 800
         <! viewBox_ "-2 -2 4 4" $ do
        circ (coord C1) (fillblue `animAdd` c1)
        circ (coord C2) (fillblue `animAdd` c2)
        circ (coord C3) (fillblue `animAdd` c3)
        circ (coord C4) (fillblue `animAdd` c4)
        circ (coord c `animSub` trans) fillblue


circlesClass :: IO (ReactClass PageState Transition AnimationState)
circlesClass =
    createClass mainView transition initialState initialAnimationState [Flash]
