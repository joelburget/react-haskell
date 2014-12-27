{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiWayIf,
  FlexibleContexts #-}
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

class Animatable a where
    -- TODO is `to` always `animZero`?
    interpolate :: Easing -> a -> a -> Double -> a
    animAdd :: a -> a -> a
    animSub :: a -> a -> a
    animZero :: a

instance Animatable Double where
    interpolate ease from to t = from + easeDouble ease t * (to - from)
    animAdd = (+)
    animSub = (-)
    animZero = 0

-- I think this could become Functor if we limit `to` to `animZero`
-- instance (Applicative f, Animatable a) => Animatable (f a) where
--     interpolate ease from to t = interpolate ease <$> from <*> to <*> pure t
--     animAdd = liftA2 animAdd
--     animZero = pure animZero

-- TODO use generics for all tuple instances
instance Animatable () where
    interpolate _ _ _ _ = ()
    animAdd _ _ = ()
    animSub _ _ = ()
    animZero = ()

instance (Animatable a, Animatable b) => Animatable (a, b) where
    interpolate ease (x0, y0) (x1, y1) t =
        (interpolate ease x0 x1 t, interpolate ease y0 y1 t)
    animAdd (x0, y0) (x1, y1) = (x0 `animAdd` x1, y0 `animAdd` y1)
    animSub (x0, y0) (x1, y1) = (x0 `animSub` x1, y0 `animSub` y1)
    animZero = (animZero, animZero)

instance (Animatable a, Animatable b, Animatable c) => Animatable (a, b, c) where
    interpolate ease (x0, y0, z0) (x1, y1, z1) t =
        (interpolate ease x0 x1 t,
         interpolate ease y0 y1 t,
         interpolate ease z0 z1 t)
    animAdd (x0, y0, z0) (x1, y1, z1) =
        (x0 `animAdd` x1,
         y0 `animAdd` y1,
         z0 `animAdd` z1)
    animSub (x0, y0, z0) (x1, y1, z1) =
        (x0 `animSub` x1,
         y0 `animSub` y1,
         z0 `animSub` z1)
    animZero = (animZero, animZero, animZero)

data Color = Color Int Int Int

intLerp :: Int -> Int -> Double -> Int
intLerp a b t = floor $ (fromIntegral a) + (fromIntegral $ b - a) * t

instance Animatable Color where
    interpolate ease (Color r0 g0 b0) (Color r1 g1 b1) t =
        let t' = interpolate ease 0 1 t
        in Color (intLerp r0 r1 t') (intLerp g0 g1 t') (intLerp b0 b1 t')
    animAdd (Color r0 g0 b0) (Color r1 g1 b1) =
        (Color (r0 + r1) (g0 + g1) (b0 + b1))
    animSub (Color r0 g0 b0) (Color r1 g1 b1) =
        (Color (r0 - r1) (g0 - g1) (b0 - b1))
    animZero = (Color 0 0 0)

instance Show Color where
    show (Color r g b) = "rgb" ++ show (r, g, b)

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
        sinFactor = sin $ (t - p / 4) * (2 * pi / p)
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

getAnimState :: Monad m => ReactT anim signal m [(RunningAnim signal anim, Double)]
getAnimState = ReactT $ \anim -> return ([], anim)

interpolate' :: Animatable anim
             => Easing
             -> [anim -> anim]
             -> Double
             -> anim
interpolate' easing mods progress =
    let modded = foldr ($) animZero mods
    in interpolate easing animZero modded progress

getWithEasing :: (Monad m, Eq animKey, Animatable (AnimTy animKey)) => animKey -> ReactT animKey signal m (AnimTy animKey)
getWithEasing name = ReactT $ \anims ->
    let relevant = filter
            (\running -> (animId . config . fst) running == name)
            anims
        mapped = map (\(RunningAnim (AnimConfig _ mod easing _ _) _, progress) ->
                     interpolate' easing mod progress) relevant
        folded = foldr animAdd animZero mapped
    in return ([], folded)

lerp :: Double -> RunningAnim signal animKey -> Double
lerp time (RunningAnim (AnimConfig duration _ _ _ _) begin) =
    (time - begin) / duration
