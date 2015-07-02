{-# LANGUAGE OverloadedStrings, TypeFamilies, ExtendedDefaultRules,
  LiberalTypeSynonyms #-}
module Slide (slideClass) where

import Control.Applicative

import Lens.Family2 hiding (view)
import React


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

view :: SlideState -> Slide React'
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

slideClass :: [AttrOrHandler Void] -> () -> ReactNode Void
slideClass = classLeaf $ smartClass
    { name = "Slide"
    , transition = transition
    , renderFn = view
    , initialState = (initialClassState, initialAnimationState )
    }
