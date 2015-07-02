{-# LANGUAGE OverloadedStrings #-}
module Main where

import React
import React.GHCJS
import Data.Void

import Circles
import Easing
-- import Nest
import Simple
-- import SimpleAnim
import Slide
import Chain

wholePage_ :: [AttrOrHandler Void] -> () -> ReactNode Void
wholePage_ = classLeaf $ dumbClass
    { name = "WholePage"
    , renderFn = \_ _ -> main_ $ do
          simpleClass_ [] ()
          circlesClass_ [] ()
          easingClass_ [] ()
          slideClass_ [] ()
          chainClass_ [] ()
    }

main :: IO ()
main = do
    Just doc <- currentDocument
    let elemId :: JSString
        elemId = "inject"
    Just elem <- documentGetElementById doc elemId
    render (wholePage_ [] ()) elem
