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

data Circles

data Circ = C1 | C2 | C3 | C4 deriving (Eq, Show)
data instance PageState Circles = CirclePageState Circ [Circ]
data instance AnimationState Circles = CircleAnimations Color Color Color Color (Double, Double)

type PageState' = PageState Circles
type AnimState = AnimationState Circles


coord :: Circ -> (Double, Double)
coord C1 = (-1, -1)
coord C2 = (1, -1)
coord C3 = (1, 1)
coord C4 = (-1, 1)


circOrder :: [Circ]
circOrder = cycle [C1, C3, C2, C4]


initialState :: PageState'
initialState = CirclePageState C1 (tail circOrder)


initialAnimationState :: AnimState
initialAnimationState =
    CircleAnimations animZero animZero animZero animZero (0, 0)


colorL :: Circ -> Lens' AnimState Color
colorL C1 f (CircleAnimations c1 c2 c3 c4 pt) =
    (\x -> CircleAnimations x c2 c3 c4 pt) <$> f c1
colorL C2 f (CircleAnimations c1 c2 c3 c4 pt) =
    (\x -> CircleAnimations c1 x c3 c4 pt) <$> f c2
colorL C3 f (CircleAnimations c1 c2 c3 c4 pt) =
    (\x -> CircleAnimations c1 c2 x c4 pt) <$> f c3
colorL C4 f (CircleAnimations c1 c2 c3 c4 pt) =
    (\x -> CircleAnimations c1 c2 c3 x pt) <$> f c4


ptL :: Lens' AnimState (Double, Double)
ptL f (CircleAnimations c1 c2 c3 c4 pt) =
    CircleAnimations c1 c2 c3 c4 <$> f pt


-- update

data instance Signal Circles = Flash
type Transition = Signal Circles

transition :: PageState'
           -> Transition
           -> (PageState' , [AnimConfig Circles])
transition (CirclePageState c' (c:cs)) Flash =
    let coordTrans = coord c `animSub` coord c'
        colorTrans = fillorange `animSub` fillblue
    in ( CirclePageState c cs
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
circ :: (Double, Double) -> Color -> React Circles ()
circ (x, y) color = circle_ <! cx_ x
                            <! cy_ y
                            <! r_ 0.15
                            <! fill_' color


mainView :: PageState' -> React Circles ()
mainView (CirclePageState c _) = div_ $ do
    CircleAnimations c1 c2 c3 c4 trans <- getAnimationState

    svg_ <! width_ 800
         <! height_ 800
         <! viewBox_ "-2 -2 4 4" $ do
        circ (coord C1) (fillblue `animAdd` c1)
        circ (coord C2) (fillblue `animAdd` c2)
        circ (coord C3) (fillblue `animAdd` c3)
        circ (coord C4) (fillblue `animAdd` c4)
        circ (coord c `animSub` trans) fillblue


circlesClass :: IO (ReactClass Circles)
circlesClass =
    createClass mainView transition initialState initialAnimationState [Flash]
