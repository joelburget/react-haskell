{-# LANGUAGE OverloadedStrings, Rank2Types, TupleSections,
  GeneralizedNewtypeDeriving, TypeFamilies #-}
module Easing (easingClass) where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Map hiding (map)
import Data.String (fromString)
import Prelude hiding (lookup)

import Haste hiding (fromString)
import Haste.JSON hiding ((!))
import React hiding (repeat)
import Lens.Family2 hiding (view)


-- model

data EasingDemo
data EasingState = Easings [Easing]
data AnimState = EasingMap (Map Easing Double)
data Transition = Restart

instance ReactKey EasingDemo where
    type ClassState EasingDemo = EasingState
    type AnimationState EasingDemo = AnimState
    type Signal EasingDemo = Transition

initialClassState :: EasingState
initialClassState = Easings easings

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

    , EaseInBounce
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

transition :: EasingState
           -> Transition
           -> (EasingState, [AnimConfig EasingDemo])
transition (Easings easings) Restart =
    ( Easings easings
    , [ AnimConfig 1000 (0, 1) (animIx easing) easing (const Nothing)
      | easing <- easings
      ]
    )

-- view

buttonBox :: React EasingDemo ()
buttonBox = div_ [ class_ "button-box" ] $
    button_ [ class_ "btn btn--m btn--gray-border"
            , onClick (const (Just Restart))
            ]
            "start easing"

view :: EasingState -> React EasingDemo ()
view (Easings easings) = div_ $ do
    EasingMap runningEasings <- getAnimationState

    buttonBox
    div_ [ class_ "easings" ] $ forM_ easings $ \easing ->
        div_ [ class_ "box" ] $ do
            subView (runningEasings ! easing) easing
            div_ [ class_ "caption" ] $ fromString $ show easing
    buttonBox

fillblue, fillorange :: Color
fillblue = Color 85 161 220
fillorange = Color 245 175 51

fill_' = fill_ . fromString . show

bounded :: Ord a => a -> a -> a -> a
bounded lower _ t | t < lower = lower
bounded _ upper t | t > upper = upper
bounded _ _ t = t

subView :: Double -> Easing -> React EasingDemo ()
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
          , transform_ (fromString ("rotate(" ++ show t ++ ")"))
          ]

    -- top right
    rect_
          [ x_ (-15)
          , y_ (-10)
          , width_ 30
          , height_ 30
          , fill_' fillblue
          , transform_ (fromString ("translate(75 25) scale(" ++ show (1 + 0.5 * t) ++ ")"))
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
          , transform_ (fromString ("translate(80 75) rotate(" ++ show (t * 90) ++ ")"))
          ]


easingClass :: IO (ReactClass EasingDemo)
easingClass =
    createClass view transition initialClassState initialAnimationState []
