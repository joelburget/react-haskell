{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.String (fromString)

import Haste hiding (fromString)
import Haste.JSON
import React

-- model

type AnimationState = Double

-- update

data Transition = Restart

transition state Restart =
    ( state
    , [AnimConfig 1000 0 1 "Anim" (const (Just Restart))]
    )

-- view

view :: () -> React AnimationState Transition ()
view () = div_ $ do
    div_ $ button_ <! onClick (const (Just Restart)) $ "click me!"

    div_ $ forM_
        [ Linear

        , EaseInQuad
        , EaseOutQuad
        , EaseInOutQuad

        , EaseInCubic
        , EaseOutCubic
        , EaseInOutCubic

        , EaseInQuart
        , EaseOutQuart
        , EaseInOutQuart

        , EaseInQuint
        , EaseOutQuint
        , EaseInOutQuint

        , EaseInElastic
        , EaseOutElastic
        -- , EaseInOutElastic

        , EaseInBounce
        , EaseOutBounce
        -- , EaseInOutBounce

        , EaseBezier 0.5 0 0.8 0.8
        , EaseInSine
        , EaseOutSine
        ] $ \easing -> div_ <! class_ "box" $ do
                animParam <- getWithEasing easing "Anim"
                subView animParam
                div_ <! class_ "caption" $ fromString $ show easing

intLerp :: Int -> Int -> Double -> Int
intLerp a b t = floor $ (fromIntegral a) + (fromIntegral $ b - a) * t

makeColor :: (Int, Int, Int) -> JSString
makeColor fill = fromString $ "rgb" ++ show fill

subView :: Double -> React AnimationState Transition ()
subView t = svg_ <! width_ 200
                 <! height_ 200
                 <! viewBox_ "0 0 100 100" $ do

    let fillblue = (85, 161, 220)
        (blueR, blueG, blueB) = fillblue
        fillorange = (245, 175, 51)
        (orangeR, orangeG, orangeB) = fillorange
        fillblue' = makeColor fillblue
        fillorange' = makeColor fillorange
        fillBetween = makeColor (intLerp blueR orangeR t,
                                 intLerp blueG orangeG t,
                                 intLerp blueB orangeB t)

    rect_ <! x_ 0
          <! y_ 0
          <! width_ (t*100)
          <! height_ 2
          <! fill_ fillblue'

    -- top left
    rect_ <! x_ 15
          <! y_ (5 + 10 * (1 - t))
          <! width_ 30
          <! height_ 40
          <! fill_ fillblue'
          <! transform_ (fromString ("rotate(" ++ show t ++ ")"))

    -- top right
    rect_
          <! x_ (-15)
          <! y_ (-10)
          <! width_ 30
          <! height_ 30
          <! fill_ fillblue'
          <! transform_ (fromString ("translate(75 25) scale(" ++ show (1 + 0.5 * t) ++ ")"))

    -- bottom left
    rect_ <! x_ 5
          <! y_ 60
          <! width_ 40
          <! height_ 30
          <! fill_ fillBetween

    -- bottom right
    rect_
          <! x_ (-7.5)
          <! y_ (-20)
          <! width_ 15
          <! height_ 40
          <! fill_ fillblue'
          <! transform_ (fromString ("translate(80 75) rotate(" ++ show (t * 90) ++ ")"))


main :: IO ()
main = do
    Just elem <- elemById "inject"
    cls <- createClass view transition () []
    render elem cls
    return ()
