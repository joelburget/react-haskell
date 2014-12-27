module Main where

import Haste
import React

import Circles
import Easing
import Simple
-- import SimpleAnim
import Slide

main :: IO ()
main = withElems
    ["simple-demo", "circles-demo", "easing-demo", "slide-demo"] $
        \[simpleNode, circlesNode, easingNode, slideNode] -> do

            render simpleNode =<< simpleClass
            render circlesNode =<< circlesClass
            render easingNode =<< easingClass
            render slideNode =<< slideClass

            return ()
