{-# LANGUAGE OverloadedStrings #-}
module Slide (slideClass) where

import Haste
import Haste.JSON
import React


-- model

data PageState = SlidOpen | SlidClosed

initialPageState :: PageState
initialPageState = SlidClosed

type AnimationState = Double

initialAnimationState :: AnimationState
initialAnimationState = 0


-- update

data Transition = Toggle

paneWidth = 200

slide :: Double -> AnimConfig Transition AnimationState
slide from = AnimConfig
    { duration = 1000
    , lens = id
    , from = from
    , easing = EaseInQuad
    , onComplete = const Nothing
    }

transition :: PageState -> Transition -> (PageState, [AnimConfig Transition AnimationState])
transition SlidOpen Toggle = (SlidClosed, [ slide paneWidth ])
transition SlidClosed Toggle = (SlidOpen, [ slide (-paneWidth) ])


-- view

view :: PageState -> React AnimationState Transition ()
view slid = div_ <! class_ "slider-container" $ do
    width <- getAnimState

    div_ $ button_ <! onClick (const (Just Toggle)) $ "click me!"
    div_ <! class_ "slider"
         <! style_ (Dict [("width", Num width)]) $ ""

slideClass :: IO (ReactClass PageState Transition AnimationState)
slideClass =
    createClass view transition initialPageState initialAnimationState []
