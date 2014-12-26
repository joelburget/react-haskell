{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiWayIf #-}
module React.Anim where

import Control.Applicative
import Data.IORef
import Data.Monoid

import Haste

import React.Imports
import React.Types

-- TODO support delays

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

    | EaseInBounce
    | EaseOutBounce
    | EaseInOutBounce

    | EaseBezier Double Double Double Double
    | EaseInSine
    | EaseOutSine
    deriving Show

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
easeInOutPow pow t = if t < 0.5
   then easeInPow pow (t * 2) / 2
   else 1 - easeInPow pow ((1 - t) * 2) / 2

elastic :: Double -> Double
elastic t =
    let p = 0.3
        powFactor = 2 ** (-10 * t)
        sinFactor = sin ( (t - p / 4) * (2 * pi / p))
    in powFactor * sinFactor + 1

easeDouble :: Easing -> Double -> Double
easeDouble Linear t = t

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

easeDouble EaseInBounce t = easeDouble EaseOutBounce (1 - t)
easeDouble EaseOutBounce t = let c = 7.5625 in
    if | t < (1 / 2.75) -> c * t * t
       | t < (2 / 2.75) -> let t' = t - (1.5 / 2.75) in c * t' * t' + 0.75
       | t < (2.5 / 2.75) -> let t' = t - (2.25 / 2.75) in c * t' * t' + 0.9375
       | otherwise -> let t' = t - (2.625 / 2.75) in c * t' * t' + 0.984375

-- TODO fix
easeDouble EaseInOutBounce t =
    if t < 0.5
        then easeDouble EaseInBounce (t * 2) / 2
        else 1 - easeDouble EaseOutBounce ((1 - t) * 2) / 2

easeDouble EaseInElastic t = 1 - elastic (1 - t)
easeDouble EaseOutElastic t = elastic t

-- TODO fix
easeDouble EaseInOutElastic t =
    if t < 0.5
       then elastic (t * 2) / 2
       else 1 - elastic ((1 - t) * 2) / 2

easeDouble (EaseBezier x0 y0 x1 y1) t = js_bezier x0 y0 x1 y1 t

-- some magic numbers i found on the internet
easeDouble EaseInSine t = js_bezier 0.47 0 0.745 0.715 t
easeDouble EaseOutSine t = js_bezier 0.39 0.575 0.565 1 t

instance Animatable Double where
    interpolate ease from to t = from + easeDouble ease t * (to - from)
    animAdd = (+)
    animZero = 0

getAnimState :: Monad m => ReactT anim signal m [RunningAnim signal]
getAnimState = ReactT $ \anim -> return ([], anim)

getWithEasing :: Monad m => Easing -> String -> ReactT anim signal m Double
getWithEasing easing name = ReactT $ \anims -> do
    let relevant = filter
            (\(RunningAnim (AnimConfig _ _ _ key _) _ _) -> key == name)
            anims
        mapped :: [Sum Double]
        mapped = map (\(RunningAnim (AnimConfig _ from to _ _) _ progress) ->
                     Sum $ interpolate easing from to progress) relevant
    return ([], getSum (mconcat mapped))

lerp' :: Double -> RunningAnim signal -> RunningAnim signal
lerp' time anim = anim{progress=lerp time anim}

lerp :: Double -> RunningAnim signal -> Double
lerp time (RunningAnim (AnimConfig duration _ _ _ _) begin _) =
    (time - begin) / duration
