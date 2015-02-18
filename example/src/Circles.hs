{-# LANGUAGE OverloadedStrings, Rank2Types, LiberalTypeSynonyms #-}
module Circles (circlesClass) where

import Control.Applicative
import Control.Monad
import Data.String (fromString)

import Lens.Family2

import Haste hiding (fromString)
import Haste.JSON
import React
import React.Anim
import React.Anim.Class


-- model

data Circ = C1 | C2 | C3 | C4 deriving (Eq, Show)
data CircState = CircState Circ [Circ]
data AnimState = AnimState Color Color Color Color (Double, Double)
data Transition = SingleFlash Circ | RepeatingFlash
type Circles a = a CircState Transition AnimState

coord :: Circ -> (Double, Double)
coord C1 = (-1, -1)
coord C2 = (1, -1)
coord C3 = (1, 1)
coord C4 = (-1, 1)


circOrder :: [Circ]
circOrder = cycle [C1, C3, C2, C4]


initialState :: CircState
initialState = CircState C1 (tail circOrder)


initialAnimationState :: AnimState
initialAnimationState =
    AnimState animZero animZero animZero animZero (0, 0)


colorL :: Circ -> Lens' AnimState Color
colorL C1 f (AnimState c1 c2 c3 c4 pt) =
    (\x -> AnimState x c2 c3 c4 pt) <$> f c1
colorL C2 f (AnimState c1 c2 c3 c4 pt) =
    (\x -> AnimState c1 x c3 c4 pt) <$> f c2
colorL C3 f (AnimState c1 c2 c3 c4 pt) =
    (\x -> AnimState c1 c2 x c4 pt) <$> f c3
colorL C4 f (AnimState c1 c2 c3 c4 pt) =
    (\x -> AnimState c1 c2 c3 x pt) <$> f c4


ptL :: Lens' AnimState (Double, Double)
ptL f (AnimState c1 c2 c3 c4 pt) = AnimState c1 c2 c3 c4 <$> f pt


-- update

transition :: Transition
           -> CircState
           -> (CircState, [AnimConfig Transition AnimState])
transition flash (CircState c' cs) =
    let colorTrans = fillorange `animSub` fillblue
        (newLoc, cs', newSignal) = case flash of
            RepeatingFlash -> (head cs, tail cs, Just RepeatingFlash)
            SingleFlash c'' -> (c'', cs, Nothing)
        coordTrans = coord newLoc `animSub` coord c'
    in ( CircState newLoc cs'
       , [ AnimConfig
               800
               (colorTrans, animZero)
               (colorL newLoc)
               EaseInQuad
               (const newSignal)
         , AnimConfig
               2000
               (coordTrans, animZero)
               ptL
               EaseInOutQuad
               (const Nothing)
         ]
       )


-- view

fillblue, fillorange :: Color
fillblue = Color 85 161 220
fillorange = Color 245 175 51


fill_' = fill_ . fromString . show


circ :: Circ -> Color -> Circles ReactA'
circ c = circ' True (const (Just (SingleFlash c))) (coord c)


circ' :: Bool
      -> (MouseEvent -> Maybe Transition)
      -> (Double, Double)
      -> Color
      -> Circles ReactA'
circ' clickable handler (x, y) color =
    let lst = [ cx_ x
              , cy_ y
              , r_ 0.15
              , fill_' color
              ]
        lst' = [ class_ "hover-circ"
               , onClick handler
               ]
               ++ lst
    in circle_ (if clickable then lst' else lst)


mainView :: CircState -> AnimState -> Circles ReactA'
mainView (CircState c _) (AnimState c1 c2 c3 c4 trans) = div_ $ do

    svg_ [ width_ 600
         , height_ 600
         , viewBox_ "-1.5 -1.5 3 3"
         ] $ do
        circ C1 (fillblue `animAdd` c1)
        circ C2 (fillblue `animAdd` c2)
        circ C3 (fillblue `animAdd` c3)
        circ C4 (fillblue `animAdd` c4)
        circ' False (const Nothing) (coord c `animSub` trans) fillblue


circlesClass :: IO (Circles ReactClassA')
circlesClass =
    createClass mainView transition initialState initialAnimationState [RepeatingFlash]
