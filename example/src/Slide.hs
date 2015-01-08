{-# LANGUAGE OverloadedStrings, TypeFamilies, ExtendedDefaultRules #-}
module Slide (slideClass) where

import Control.Applicative

import Haste
import Haste.JSON
import Lens.Family2 hiding (view)
import React


-- model

data Slide
data SlideState = Open | Closed
data Toggle = Toggle deriving Show

instance ReactKey Slide where
    type ClassState Slide = SlideState
    type Signal Slide = Toggle
    type AnimationState Slide = Double

initialClassState :: SlideState
initialClassState = Closed

initialAnimationState :: Double
initialAnimationState = 0

-- update

paneWidth = 200

slide :: Double -> AnimConfig Slide
slide from = AnimConfig
    { duration = 1000
    , lens = id
    , endpoints = (from, 0)
    , easing = EaseInOutQuad
    , onComplete = const Nothing
    }

transition :: Toggle -> SlideState -> (SlideState, [AnimConfig Slide])
transition Toggle Open = (Closed, [ slide paneWidth ])
transition Toggle Closed = (Open, [ slide (-paneWidth) ])


-- view

view :: SlideState -> React Slide ()
view slid = div_ [ class_ "slider-container" ] $ do
    animWidth <- getAnimationState
    let inherentWidth = case slid of
            Open -> paneWidth
            Closed -> 0

    div_ $ button_ [ class_ "btn btn--m btn--gray-border", onClick (const (Just Toggle)) ] "toggle"
    div_ [ class_ "slider"
         , style_ (Dict [("width", Num (inherentWidth + animWidth))])
         ]
         ""

slideClass :: IO (ReactClass Slide)
slideClass =
    createClass view transition initialClassState initialAnimationState []
