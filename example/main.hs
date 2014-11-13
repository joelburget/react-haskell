{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haste
import React
import Prelude hiding (div)

main = do
    Just elem <- elemById "inject"
    renderComponent elem $ div <! className "foo" $ do
        "some string"

        pre "this thing should be in a pre"

        "some other string"
