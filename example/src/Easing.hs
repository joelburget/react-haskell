{-# LANGUAGE OverloadedStrings, Rank2Types, TupleSections,
  GeneralizedNewtypeDeriving, TypeFamilies, LiberalTypeSynonyms #-}
module Easing (easingClass) where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Map hiding (map)
import Data.String (fromString)
import Prelude hiding (lookup)

import Haste hiding (fromString)
import Haste.JSON hiding ((!))
import React hiding (repeat)
import React.Anim
import React.Anim.Class

import Lens.Family2 hiding (view)

-- model

data Eased = Open | Closed deriving Eq
data EasingState = Easings Eased [Easing]
data AnimState = EasingMap (Map Easing Double)
data Transition = Toggle
type Ease a = a EasingState Transition AnimState

initialClassState :: EasingState
initialClassState = Easings Closed easings

initialAnimationState :: AnimState
initialAnimationState = EasingMap $ fromList $ zip easings (repeat 0)

easings :: [Easing]
easings =
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

    -- , EaseInBounce
    , EaseOutBounce
    -- , EaseInOutBounce

    , EaseBezier 0.5 0 0.8 0.8
    , EaseInSine
    , EaseOutSine
    ]

-- update

animIx :: Easing -> Lens' (AnimState) Double
animIx easing f (EasingMap m) = EasingMap <$>
    ((\v' -> insert easing v' m) <$> f (m ! easing))

transition :: Transition
           -> EasingState
           -> (EasingState, [AnimConfig Transition AnimState])
transition Toggle (Easings Closed easings) =
    ( Easings Open easings
    , [ AnimConfig 1000 (-1, 0) (animIx easing) easing (const Nothing)
      | easing <- easings
      ]
    )
transition Toggle (Easings Open easings) =
    ( Easings Closed easings
    , [ AnimConfig 1000 (1, 0) (animIx easing) easing (const Nothing)
      | easing <- easings
      ]
    )

-- view

buttonBox :: Ease ReactA'
buttonBox = div_ [ class_ "button-box" ] $
    button_ [ class_ "btn btn--m btn--gray-border"
            , onClick (const (Just Toggle))
            ]
            "toggle easing"

view :: EasingState -> AnimState -> Ease ReactA'
view (Easings direction easings) (EasingMap runningEasings) = div_ $ do
    let t = if direction == Closed then 0 else 1

    buttonBox
    div_ [ class_ "easings" ] $ forM_ easings $ \easing ->
        div_ [ class_ "box" ] $ do
            subView (t + (runningEasings ! easing)) easing
            -- div_ [ class_ "caption" ] $ fromString $ show easing
            div_ [ class_ "caption" ] $ do
                let e = fromString $ show easing
                    e' = if e == "EaseBezier (--0.5) (-0.0) (--0.8) (--0.8)"
                    then "EaseBezier 0.5 0.0 0.8 0.8"
                    else e
                text_ e'
    buttonBox

fillblue, fillorange :: Color
fillblue = Color 85 161 220
fillorange = Color 245 175 51

fill_' = fill_ . fromString . show

bounded :: Ord a => a -> a -> a -> a
bounded lower _ t | t < lower = lower
bounded _ upper t | t > upper = upper
bounded _ _ t = t

safeShow :: Show a => a -> String
safeShow x =
    let shown = show x
    in if take 2 shown == "--" then drop 2 shown else shown

-- Trying to replicate http://www.objc.io/issue-12/view-layer-synergy.html
subView :: Double -> Easing -> Ease ReactA'
subView t easing = svg_ [ width_ 100
                        , height_ 100
                        , viewBox_ "0 0 100 100"
                        ] $ do

    rect_ [ x_ 0
          , y_ 0
          , width_ (bounded 0 1000 (t*100))
          , height_ 2
          , fill_' fillblue
          ]

    -- top left
    rect_ [ x_ 15
          , y_ (5 + 10 * (1 - t))
          , width_ 30
          , height_ 40
          , fill_' fillblue
          , transform_ (fromString ("translate(0 " ++ safeShow t ++ ")"))
          ]

    -- top right
    rect_
          [ x_ (-15)
          , y_ (-10)
          , width_ 30
          , height_ 30
          , fill_' fillblue
          , transform_ (fromString ("translate(75 25) scale(" ++ safeShow (1 + 0.5 * t) ++ ")"))
          ]

    -- bottom left
    rect_ [ x_ 5
          , y_ 60
          , width_ 40
          , height_ 30
          , fill_' (interpolate Linear fillblue fillorange t)
          ]

    -- bottom right
    rect_
          [ x_ (-7.5)
          , y_ (-20)
          , width_ 15
          , height_ 40
          , fill_' fillblue
          , transform_ (fromString ("translate(80 75) rotate(" ++ safeShow (t * 90) ++ ")"))
          ]


easingClass :: IO (Ease ReactClassA')
easingClass =
    createClass view transition initialClassState initialAnimationState []
