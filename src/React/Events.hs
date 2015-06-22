{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module React.Events where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Maybe

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Prim
import GHCJS.Types

import React.Imports
import React.Types


-- XXX isn't this in GHCJS.Prim?
instance Eq JSString where
    (==) = eqRef

-- TODO: handle (a -> Maybe b) or (a -> b)

onChange :: (ChangeEvent -> Maybe s) -> AttrOrHandler s
onChange = mkEventHandler ChangeEvt

onKeyDown :: (KeyboardEvent -> Maybe s) -> AttrOrHandler s
onKeyDown = mkEventHandler KeyDownEvt

onKeyPress :: (KeyboardEvent -> Maybe s) -> AttrOrHandler s
onKeyPress = mkEventHandler KeyPressEvt

onKeyUp :: (KeyboardEvent -> Maybe s) -> AttrOrHandler s
onKeyUp = mkEventHandler KeyUpEvt

onMouseEnter :: (MouseEvent -> Maybe s) -> AttrOrHandler s
onMouseEnter = mkEventHandler MouseEnterEvt

onMouseLeave :: (MouseEvent -> Maybe s) -> AttrOrHandler s
onMouseLeave = mkEventHandler MouseLeaveEvt

onDoubleClick :: (MouseEvent -> Maybe s) -> AttrOrHandler s
onDoubleClick = mkEventHandler DoubleClickEvt

onClick :: (MouseEvent -> Maybe s) -> AttrOrHandler s
onClick = mkEventHandler ClickEvt

onEnter :: s -> AttrOrHandler s
onEnter s = onKeyPress handler where
    handler KeyboardEvent{key="Enter"} = Just s
    handler _ = Nothing
