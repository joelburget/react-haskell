{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Slide (slideClass) where

import Control.Applicative

import Haste
import Haste.JSON
import Lens.Family2 hiding (view)
import React


-- model

data Slide
data instance PageState Slide = Open | Closed

initialPageState :: PageState'
initialPageState = Closed

data instance AnimationState Slide = SlidingProgress Double

initialAnimationState :: AnimState
initialAnimationState = SlidingProgress 0

type PageState' = PageState Slide
type AnimState = AnimationState Slide
type Sig = Signal Slide

-- update

data instance Signal Slide = Toggle

animLens :: Lens' AnimState Double
animLens f (SlidingProgress t) = SlidingProgress <$> f t

paneWidth = 200

slide :: Double -> Easing -> AnimConfig Slide
slide from easing = AnimConfig
    { duration = 1000
    , lens = animLens
    , from = from
    , easing = easing
    , onComplete = const Nothing
    }

transition :: PageState' -> Sig -> (PageState', [AnimConfig Slide])
transition Open Toggle = (Closed, [ slide paneWidth EaseInOutQuad ])
transition Closed Toggle = (Open, [ slide (-paneWidth) EaseInOutQuad ])


-- view

view :: PageState' -> React Slide ()
view slid = div_ <! class_ "slider-container" $ do
    SlidingProgress animWidth <- getAnimationState
    let inherentWidth = case slid of
            Open -> paneWidth
            Closed -> 0

    div_ $ button_ <! onClick (const (Just Toggle)) $ "toggle"
    div_ <! class_ "slider"
         <! style_ (Dict [("width", Num (inherentWidth + animWidth))]) $ ""

slideClass :: IO (ReactClass Slide)
slideClass =
    createClass view transition initialPageState initialAnimationState []
