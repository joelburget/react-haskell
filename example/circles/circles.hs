{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.String (fromString)

import Haste hiding (fromString)
import Haste.JSON
import React

-- model

data Circ = C1 | C2 | C3 | C4
data PageState = PageState Circ [Circ]
type AnimationState = (Double, Double)

colorName :: Circ -> String
colorName C1 = "Color1"
colorName C2 = "Color2"
colorName C3 = "Color3"
colorName C4 = "Color4"

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

data Transition = Flash

transition (PageState c' (c:cs)) Flash =
    let (x0, y0) = coord c'
        (x1, y1) = coord c
    in ( PageState c cs
       , [ AnimConfig 800 1 0 (colorName c) (const (Just Flash))
         , AnimConfig 2000 (x0 - x1) 0 "CircX" (const Nothing)
         , AnimConfig 2000 (y0 - y1) 0 "CircY" (const Nothing)
         ]
       )

-- view

intLerp :: Int -> Int -> Double -> Int
intLerp a b t = floor $ (fromIntegral a) + (fromIntegral $ b - a) * t

fillblue = (85, 161, 220)
(blueR, blueG, blueB) = fillblue
fillorange = (245, 175, 51)
(orangeR, orangeG, orangeB) = fillorange

-- PureReact ()
circ :: (Double, Double) -> Double -> React AnimationState Transition ()
circ (x, y) t =
    let color = fromString $ "rgb" ++ show (intLerp blueR orangeR t,
                                            intLerp blueG orangeG t,
                                            intLerp blueB orangeB t)
    in circle_ <! cx_ x
               <! cy_ y
               <! r_ 0.15
               <! fill_ color

view :: PageState -> React AnimationState Transition ()
view (PageState c _) = div_ $ do
    let (cx0, cy0) = coord c
    cx1 <- getWithEasing EaseInOutQuad "CircX"
    cy1 <- getWithEasing EaseInOutQuad "CircY"

    c1 <- getWithEasing EaseInQuad "Color1"
    c2 <- getWithEasing EaseInQuad "Color2"
    c3 <- getWithEasing EaseInQuad "Color3"
    c4 <- getWithEasing EaseInQuad "Color4"

    svg_ <! width_ 800
         <! height_ 800
         <! viewBox_ "-2 -2 4 4" $ do
        circ (coord C1) c1
        circ (coord C2) c2
        circ (coord C3) c3
        circ (coord C4) c4
        circ ((cx0 + cx1) / 2, (cy0 + cy1) / 2) 0

main :: IO ()
main = do
    Just elem <- elemById "inject"
    cls <- createClass view transition initialState [Flash]
    render elem cls
    return ()
