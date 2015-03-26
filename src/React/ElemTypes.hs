module React.ElemTypes where

import GHCJS.Types

import React.Types
import React.Imports

-- Useful for defining elements

foreignParent :: TermParent t
              => ForeignRender
              -> TermParentArg t
              -> t
foreignParent = termParent


reactParent :: TermParent t
            => JSString
            -> TermParentArg t
            -> t
reactParent name = termParent (js_React_DOM_parent name)


termLeaf :: Monad m
         => ForeignRender
         -> [AttrOrHandler sig]
         -> ReactT state sig m ()
termLeaf render attrs = ReactT $ do
    let (hs, as) = separateAttrs attrs
    return ([Leaf render as hs], ())


foreignLeaf :: Monad m
            => ForeignRender
            -> [AttrOrHandler sig]
            -> ReactT state sig m ()
foreignLeaf = termLeaf


reactLeaf :: Monad m
         => JSString
         -> [AttrOrHandler sig]
         -> ReactT state sig m ()
reactLeaf name = termLeaf (\as' _ -> js_React_DOM_leaf name as')

