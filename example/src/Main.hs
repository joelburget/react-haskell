module Main where

import Haste
import React

import Circles
import Easing
import Nest
import Simple
-- import SimpleAnim
import Slide

main :: IO ()
main = withElems
    ["simple-demo", "circles-demo", "easing-demo", "slide-demo", "nest-demo"] $
        \[simpleNode, circlesNode, easingNode, slideNode, nestNode] -> do

            render simpleNode =<< simpleClass
            render circlesNode =<< circlesClass
            render easingNode =<< easingClass
            render slideNode =<< slideClass
            render nestNode =<< nestClass

            return ()
