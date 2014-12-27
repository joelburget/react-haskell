{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Main where

import Control.Monad
import Data.String (fromString)

import Haste hiding (fromString)
import Haste.JSON
import React

-- model

data Circ = C1 | C2 | C3 | C4
          deriving Eq
data PageState = PageState Circ [Circ]
data AnimationState = AnimationState Color (Double, Double)
data AnimKey
    = AnimFlash Circ -- ~ Color
    | AnimMiddleCirc -- ~ (Double, Double)
    deriving Eq
type instance AnimTy AnimKey = AnimationState

-- instance (Animatable a, Animatable b) => Animatable (a, b) where
--     interpolate ease (x0, y0) (x1, y1) t =
--         (interpolate ease x0 x1 t, interpolate ease y0 y1 t)
--     animAdd (x0, y0) (x1, y1) = (x0 `animAdd` x1, y0 `animAdd` y1)
--     animSub (x0, y0) (x1, y1) = (x0 `animSub` x1, y0 `animSub` y1)
--     animZero = (animZero, animZero)

-- Use lens to modify all parts of a possible animation

coord :: Circ -> (Double, Double)
coord C1 = (-1, -1)
coord C2 = (1, -1)
coord C3 = (1, 1)
coord C4 = (-1, 1)

clockwise :: [Circ]
clockwise = cycle [C1, C3, C2, C4]

initialState :: PageState
initialState = PageState C1 (tail clockwise)

-- update

modColor :: Color -> AnimationState -> AnimationState
modColor c (AnimationState color point) = AnimationState (color `animAdd` c) point

modPt :: (Double, Double) -> AnimationState -> AnimationState
modPt pt (AnimationState color point) = AnimationState color (point `animAdd` pt)

data Transition = Flash

transition :: PageState
           -> Transition
           -> (PageState, [AnimConfig Transition AnimKey])
transition (PageState c' (c:cs)) Flash =
    let (x0, y0) = coord c'
        (x1, y1) = coord c
    in ( PageState c cs
       , [ AnimConfig 800 [modColor fillblue] EaseInQuad (AnimFlash c) (const (Just Flash))
         , AnimConfig 2000 [modPt (x0 - x1, y0 - y1)] EaseInOutQuad AnimMiddleCirc (const Nothing)
         ]
       )

-- view

fillblue, fillorange :: Color
fillblue = Color 85 161 220
fillorange = Color 245 175 51

-- PureReact ()
circ :: (Double, Double) -> Color -> React AnimKey Transition ()
circ (x, y) color = circle_ <! cx_ x
                            <! cy_ y
                            <! r_ 0.15
                            <! fill_ (fromString (show color))

view :: PageState -> React AnimKey Transition ()
view (PageState c _) = div_ $ do
    let (cx0, cy0) = coord c
    AnimationState _ (cx1, cy1) <- getWithEasing AnimMiddleCirc

    AnimationState c1 _ <- getWithEasing (AnimFlash C1)
    AnimationState c2 _ <- getWithEasing (AnimFlash C2)
    AnimationState c3 _ <- getWithEasing (AnimFlash C3)
    AnimationState c4 _ <- getWithEasing (AnimFlash C4)

    svg_ <! width_ 800
         <! height_ 800
         <! viewBox_ "-2 -2 4 4" $ do
        circ (coord C1) c1
        circ (coord C2) c2
        circ (coord C3) c3
        circ (coord C4) c4
        circ ((cx0 + cx1) / 2, (cy0 + cy1) / 2) animZero

main :: IO ()
main = do
    Just elem <- elemById "inject"
    cls <- createClass view transition initialState [Flash] :: IO (ReactClass PageState Transition AnimKey)
    render elem cls
    return ()
