module Test where

-- test:
-- * embedding static in dynamic
-- * embedding dynamic in static
-- * requiring keys in dynamic
-- * not requiring keys in static
-- * not mixing static and dynamic

import React

keyFromInt :: Int -> AttrOrHandler a
keyFromInt = key_ . fromString . show

staticInDynamic = div_ $ keyed $
    let node = span_ "static"
    in [ (key, node) | key <- [1..10] ]

dynamicInStatic = div_ $
    "this"
    "is"
    "static"
    div_ $ keyed $
        let node = span_ "static"
        in [ (key, node) | key <- [1..10] ]
    "... mostly."

-- XXX how can we tell whether we're using a parameter?
-- Answer? Things *only* render when their parents render. Thus their
-- parents know when their children's props *can* change. We make anything
-- dependent on a variable that can change dynamic.
-- oilAndWater = div_ $
