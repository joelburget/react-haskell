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
data CircState = CircState Circ [Circ]
data AnimState = AnimState Color Color Color Color (Double, Double)
data Transition = Flash

instance ReactKey Circles where
    type ClassState Circles = CircState
    type AnimationState Circles = AnimState
    type Signal Circles = Transition


coord :: Circ -> (Double, Double)
coord C1 = (-1, -1)
coord C2 = (1, -1)
coord C3 = (1, 1)
coord C4 = (-1, 1)


circOrder :: [Circ]
circOrder = cycle [C1, C3, C2, C4]


initialState :: CircState
initialState = CircState C1 (tail circOrder)


initialAnimationState :: AnimState
initialAnimationState =
    AnimState animZero animZero animZero animZero (0, 0)


colorL :: Circ -> Lens' AnimState Color
colorL C1 f (AnimState c1 c2 c3 c4 pt) =
    (\x -> AnimState x c2 c3 c4 pt) <$> f c1
colorL C2 f (AnimState c1 c2 c3 c4 pt) =
    (\x -> AnimState c1 x c3 c4 pt) <$> f c2
colorL C3 f (AnimState c1 c2 c3 c4 pt) =
    (\x -> AnimState c1 c2 x c4 pt) <$> f c3
colorL C4 f (AnimState c1 c2 c3 c4 pt) =
    (\x -> AnimState c1 c2 c3 x pt) <$> f c4


ptL :: Lens' AnimState (Double, Double)
ptL f (AnimState c1 c2 c3 c4 pt) =
    AnimState c1 c2 c3 c4 <$> f pt


-- update

transition :: CircState
           -> Transition
           -> (CircState , [AnimConfig Circles])
transition (CircState c' (c:cs)) Flash =
    let coordTrans = coord c `animSub` coord c'
        colorTrans = fillorange `animSub` fillblue
    in ( CircState c cs
       , [ AnimConfig 800 (colorTrans, animZero) (colorL c) EaseInQuad (const (Just Flash))
         , AnimConfig 2000 (coordTrans, animZero) ptL EaseInOutQuad (const Nothing)
         ]
       )


-- view

fillblue, fillorange :: Color
fillblue = Color 85 161 220
fillorange = Color 245 175 51


fill_' = fill_ . fromString . show


-- PureReact ()
circ :: (Double, Double) -> Color -> React Circles ()
circ (x, y) color = circle_ [ cx_ x
                            , cy_ y
                            , r_ 0.15
                            , fill_' color
                            ]


mainView :: CircState -> React Circles ()
mainView (CircState c _) = div_ $ do
    AnimState c1 c2 c3 c4 trans <- getAnimationState

    svg_ [ width_ 600
         , height_ 600
         , viewBox_ "-1.5 -1.5 3 3"
         ] $ do
        circ (coord C1) (fillblue `animAdd` c1)
        circ (coord C2) (fillblue `animAdd` c2)
        circ (coord C3) (fillblue `animAdd` c3)
        circ (coord C4) (fillblue `animAdd` c4)
        circ (coord c `animSub` trans) fillblue


circlesClass :: IO (ReactClass Circles)
circlesClass =
    createClass mainView transition initialState initialAnimationState [Flash]
