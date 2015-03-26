{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe (fromJust)

import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Types (Document)
import GHCJS.DOM.Document (documentGetElementById)
import GHCJS.Types (JSString)

import React
import React.Class

-- import Circles
-- import Easing
-- import Nest
import Simple
-- import SimpleAnim
-- import Slide
-- import Chain

import GHCJS.Foreign

doRender :: ReactClass a b -> JSString -> Document -> IO ()
doRender cls nodeName doc = void $ join $
    render <$> (fromJust <$> documentGetElementById doc nodeName) <*> pure cls

doRender' :: IO (ReactClass a b) -> JSString -> Document -> IO ()
doRender' cls str doc = join $ doRender <$> cls <*> pure str <*> pure doc

main :: IO ()
main = do
    Just doc <- currentDocument
    forM_
        [ ("simple-demo", doRender' simpleClass)
        -- , ("circles-demo", doRender' circlesClass)
        -- , ("easing-demo", doRender' easingClass)
        -- , ("slide-demo", doRender' slideClass)
        -- , ("chain-demo", doRender' chainClass)
        ] $ \(name, rndr) -> rndr name doc
