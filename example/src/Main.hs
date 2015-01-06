module Main where

import Haste
import React

import Circles
import Easing
-- import Nest
import Simple
-- import SimpleAnim
import Slide
import Chain

main :: IO ()
main = withElems
    ["simple-demo", "circles-demo", "easing-demo", "slide-demo", "chain-demo"] $
        \[simpleNode, circlesNode, easingNode, slideNode, chainNode] -> do

            render simpleNode =<< simpleClass
            render circlesNode =<< circlesClass
            render easingNode =<< easingClass
            render slideNode =<< slideClass
            render chainNode =<< chainClass

            return ()
