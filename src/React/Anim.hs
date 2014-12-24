{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module React.Anim where

import Control.Applicative
import Data.IORef
import Data.Monoid

import Haste

import React.Imports
import React.Types

-- react-tween-state:
-- https://github.com/chenglou/react-tween-state/blob/master/index.js

-- TODO look at velocity

-- TODO also `Floating (Scalar v)` ?
--      Double ~ Scalar v?
-- TODO common pattern:
--      from .+^ (?? *^ (to .-. from))
{-
easingFunc :: (AffineSpace p, v ~ Diff p, VectorSpace v)
           => Easing -> a -> a -> Double -> a
easingFunc Linear from to t = from .+^ (t *^ (to .-. from))
-- easingFunc Linear from to t = alerp from to t
easingFunc EaseInQuad from to t = from .+^ ((t*t) *^ (to .-. from))
easingFunc _ _ _ _ = error "that easing function has not been defined yet"
-}

data Easing
    = Linear

    | EaseInQuad
    | EaseOutQuad
    | EaseInOutQuad

    | EaseInCubic
    | EaseOutCubic
    | EaseInOutCubic

    | EaseInQuart
    | EaseOutQuart
    | EaseInOutQuart

    | EaseInQuint
    | EaseOutQuint
    | EaseInOutQuint

    | EaseInElastic
    | EaseOutElastic
    | EaseInOutElastic

    | EaseBezier Double Double Double Double
    | EaseInSine
    | EaseOutSine

class Animatable a where
    -- TODO is `to` always `animZero`?
    interpolate :: Easing -> a -> a -> Double -> a
    animAdd :: a -> a -> a
    animZero :: a

instance Animatable () where
    interpolate _ _ _ _ = ()
    animAdd _ _ = ()
    animZero = ()

-- I think this could become Functor if we limit `to` to `animZero`
instance (Applicative f, Animatable a) => Animatable (f a) where
    interpolate ease from to t = interpolate ease <$> from <*> to <*> pure t
    animAdd = liftA2 animAdd
    animZero = pure animZero

easeInPow :: Int -> Double -> Double
easeInPow pow t = t ^^ pow

easeOutPow :: Int -> Double -> Double
easeOutPow pow t = 1 - easeInPow pow (1 - t)

easeInOutPow :: Int -> Double -> Double
easeInOutPow pow t =
    let twoPow = 2 ^^ pow
    in if t < 0.5
       then easeInPow pow (t * twoPow) / twoPow
       else easeOutPow pow (t * twoPow) / twoPow

elastic :: Double -> Double
elastic t =
    let p = 0.3
        powFactor = 2 ** (-10 * t)
        sinFactor = sin ( (t - p / 4) * (2 * pi / p))
    in powFactor * sinFactor + 1

easeDouble :: Easing -> Double -> Double
easeDouble Linear t = t

{-
-- TODO do a better job of these simple easings, e.g:
easeDouble EaseInQuad t = easeInPow 2 t
easeDouble EaseOutQuad t = 1 - easeDouble EaseInQuad (1 - t)
easeDouble EaseInOutQuad t = if t < 0.5
    then (easeDouble EaseInQuad (t * 2)) / 2
    else 1 - (easeDouble EaseInQuad ((1 - t) * 2)) / 2

easeDouble EaseInCubic t = t * t * t
easeDouble EaseOutCubic t = let t' = (t - 1) in t' * t' * t' + 1
easeDouble EaseInOutCubic t = if t < 0.5
    then 4 * t * t * t
    else (t - 1) * (2 * t - 2) * (2 * t - 2) + 1

easeDouble EaseInQuart t = t * t * t * t
easeDouble EaseOutQuart t = let t' = (t - 1) in 1 - t' * t' * t' * t'
easeDouble EaseInOutQuart t = if t < 0.5
    then 8 * t * t * t * t
    else let t' = t - 1 in 1 - 8 * t' * t' * t' * t'

easeDouble EaseInQuint t = t * t * t * t * t
easeDouble EaseOutQuint t = let t' = (t - 1) in 1 + t' * t' * t' * t' * t
easeDouble EaseInOutQuint t = if t < 0.5
    then 16 * t * t * t * t * t
    else let t' = t - 1 in 1 + 16 * t' * t' * t' * t' * t
-}
easeDouble EaseInQuad t    = easeInPow 2 t
easeDouble EaseOutQuad t   = easeOutPow 2 t
easeDouble EaseInOutQuad t = easeInOutPow 2 t

easeDouble EaseInCubic t    = easeInPow 3 t
easeDouble EaseOutCubic t   = easeOutPow 3 t
easeDouble EaseInOutCubic t = easeInOutPow 3 t

easeDouble EaseInQuart t    = easeInPow 4 t
easeDouble EaseOutQuart t   = easeOutPow 4 t
easeDouble EaseInOutQuart t = easeInOutPow 4 t

easeDouble EaseInQuint t    = easeInPow 5 t
easeDouble EaseOutQuint t   = easeOutPow 5 t
easeDouble EaseInOutQuint t = easeInOutPow 5 t

easeDouble EaseInElastic t = 1 - elastic (1 - t)
easeDouble EaseOutElastic t = elastic t
easeDouble EaseInOutElastic t =
    if t < 0.5
       then (1 - elastic (1 - t * 2)) / 2
       else elastic (t * 2) / 2

easeDouble (EaseBezier x0 y0 x1 y1) t = js_bezier x0 y0 x1 y1 t

-- some magic numbers i found on the internet
easeDouble EaseInSine t = js_bezier 0.47 0 0.745 0.715 t
easeDouble EaseOutSine t = js_bezier 0.39 0.575 0.565 1 t

instance Animatable Double where
    interpolate ease from to t = from + easeDouble ease t * (to - from)
    animAdd = (+)
    animZero = 0

-- TODO getEasing :: Easing -> ident -> React Double

-- why do we use getEasing?
-- because of additive animations?
--
-- without getEasing animations (a t:0.5 easing:Quad) and (b: t:0.5 easing:Quad) go to (c: t:0.5 easing:Quad)
-- with
--
-- bullshit
-- getEasing :: Easing -> ident -> React Double
-- getEasing

getAnimState :: Monad m => ReactT anim signal m [RunningAnim signal]
getAnimState = ReactT $ \anim -> return ([], anim)

getWithEasing :: Monad m => Easing -> String -> ReactT anim trans m Double
getWithEasing easing name = ReactT $ \anims -> do
    let relevant = filter
            (\(RunningAnim (AnimConfig _ str _) _ _) -> str == name)
            anims
        mapped :: [Sum Double]
        mapped = map (\(RunningAnim _ _ progress) ->
                     Sum $ interpolate easing 1 0 progress) relevant
    return ([], getSum (mconcat mapped))

lerp' :: Double -> RunningAnim trans -> RunningAnim trans
lerp' time anim = anim{progress=lerp time anim}

lerp :: Double -> RunningAnim trans -> Double
lerp time (RunningAnim (AnimConfig duration _ _) begin _) =
    (time - begin) / duration
