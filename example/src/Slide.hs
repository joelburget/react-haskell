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

type instance ClassState Slide = SlideState
type instance Signal Slide = Toggle
type instance AnimationState Slide = Double

initialClassState :: SlideState
initialClassState = Closed

initialAnimationState :: Double
initialAnimationState = 0

-- update

paneWidth = 200

slide :: Double -> Easing -> AnimConfig Slide
slide from easing = AnimConfig
    { duration = 1000
    , lens = id
    , from = from
    , easing = easing
    , onComplete = const Nothing
    }

transition :: SlideState -> Toggle -> (SlideState, [AnimConfig Slide])
transition Open Toggle = (Closed, [ slide paneWidth EaseInOutQuad ])
transition Closed Toggle = (Open, [ slide (-paneWidth) EaseInOutQuad ])


-- view

view :: SlideState -> React Slide ()
view slid = div_ [ class_ "slider-container" ] $ do
    animWidth <- getAnimationState
    let inherentWidth = case slid of
            Open -> paneWidth
            Closed -> 0

    div_ $ button_ [ onClick (const (Just Toggle)) ] "toggle"
    div_ [ class_ "slider"
         , style_ (Dict [("width", Num (inherentWidth + animWidth))])
         ]
         ""

slideClass :: IO (ReactClass Slide)
slideClass =
    createClass view transition initialClassState initialAnimationState []
