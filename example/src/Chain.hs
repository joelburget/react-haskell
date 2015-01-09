{-# LANGUAGE OverloadedStrings, TypeFamilies, ExtendedDefaultRules #-}
module Chain (chainClass) where

import Control.Applicative
import Data.String (fromString)

import Haste hiding (fromString)
import Haste.JSON
import Lens.Family2 hiding (view)
import React


-- model

data Chain
data ChainState = Open | Closed deriving Eq
data Toggle = Toggle deriving Show

instance ReactKey Chain where
    type ClassState Chain = ChainState
    type Signal Chain = Toggle
    type AnimationState Chain = Double

initialClassState :: ChainState
initialClassState = Closed

initialAnimationState :: Double
initialAnimationState = 0

-- update

chain :: Double -> AnimConfig Chain
chain from = AnimConfig
    { duration = 1000
    , lens = id
    , endpoints = (from, 0)
    , easing = EaseInOutQuad
    , onComplete = const Nothing
    }

transition :: Toggle -> ChainState -> (ChainState, [AnimConfig Chain])
transition Toggle Open = (Closed, [ chain 1 ])
transition Toggle Closed = (Open, [ chain (-1) ])


-- view

startWidth, startHeight, finalWidth, finalHeight :: Double
startWidth = 20
startHeight = 20
finalWidth = 400
finalHeight = 200

startColor = Color 74 74 182
endColor = Color 74 178 182

-- Note:
--
-- This is not necessarily how I recommend doing compound animations. This
-- is pretty low level - I'd consider it a hack, almost. I need to think
-- more about how this should be done, but this serves my purposes for
-- a quick demo.
derive :: Double -> (Double, Double)
derive t | t == 0 = (startWidth, startHeight)
derive t | t < 1/3 =
    ( interpolate EaseInOutQuad startWidth finalWidth (t * 1.5)
    , startHeight
    )
derive t | t < 2/3 =
    ( interpolate EaseInOutQuad startWidth finalWidth (t * 1.5)
    , interpolate EaseInOutQuad startHeight finalHeight ((t - 1/3) * 1.5)
    )
derive t | t < 1 =
    ( finalWidth
    , interpolate EaseInOutQuad startHeight finalHeight ((t - 1/3) * 1.5)
    )
derive t = (finalWidth, finalHeight)

view :: ChainState -> React Chain ()
view status = div_ [ class_ "chain-container" ] $ do
    animState <- getAnimationState

    let numStatus = if status == Open then 1 else 0
        t = animState + numStatus
        (xt, yt) = derive t
        color = fromString $ show $ interpolate Linear startColor endColor t

    div_ $ button_ [ class_ "btn btn--m btn--gray-border"
                   , onClick (const (Just Toggle))
                   ] "toggle"

    div_ [ class_ "chaining"
         , style_ (Dict [("width", Num xt),
                         ("height", Num yt),
                         ("backgroundColor", color)
                        ])
         ]
         ""

chainClass :: IO (ReactClass Chain)
chainClass =
    createClass view transition initialClassState initialAnimationState []
