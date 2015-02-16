{-# LANGUAGE OverloadedStrings, TypeFamilies, ExtendedDefaultRules,
  LiberalTypeSynonyms #-}
module Slide (slideClass) where

import Control.Applicative

import Haste
import Haste.JSON
import Lens.Family2 hiding (view)
import React
import React.Anim
import React.Anim.Class


-- model

data SlideState = Open | Closed
data Toggle = Toggle deriving Show
type Slide a = a SlideState Toggle Double

initialClassState :: SlideState
initialClassState = Closed

initialAnimationState :: Double
initialAnimationState = 0

-- update

paneWidth = 200

slide :: Double -> AnimConfig Toggle Double
slide from = AnimConfig
    { duration = 1000
    , lens = id
    , endpoints = (from, 0)
    , easing = EaseInOutQuad
    , onComplete = const Nothing
    }

transition :: Toggle -> SlideState -> (SlideState, [AnimConfig Toggle Double])
transition Toggle Open = (Closed, [ slide paneWidth ])
transition Toggle Closed = (Open, [ slide (-paneWidth) ])


-- view

view :: SlideState -> Double -> Slide ReactA'
view slid animWidth = div_ [ class_ "slider-container" ] $ do

    let inherentWidth = case slid of
            Open -> paneWidth
            Closed -> 0

    div_ $ button_ [ class_ "btn btn--m btn--gray-border", onClick (const (Just Toggle)) ] "toggle"
    div_ [ class_ "slider"
         , style_ (Dict [("width", Num (inherentWidth + animWidth))])
         ]
         ""

slideClass :: IO (Slide ReactClassA')
slideClass =
    createClass view transition initialClassState initialAnimationState []
