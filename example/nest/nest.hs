{-# LANGUAGE OverloadedStrings, NamedFieldPuns, Rank2Types, TupleSections #-}
module Main where

import Data.Maybe
import Haste
import React
import Prelude hiding (fst, snd)
import Control.Applicative

import Haste.JSON
import Haste.Prim

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
    [x] -> Just x
    _ -> Nothing

iToS :: Maybe Int -> JSString
iToS Nothing = ""
iToS (Just i) = toJSStr $ show i

sToI :: JSString -> Maybe Int
sToI = readMay . fromJSStr

leftLens :: MockLens PageState (Maybe Int)
leftLens f (l, r) = (,r) <$> f l

rightLens :: MockLens PageState JSString
rightLens f (l, r) = (l,) <$> f r

--

type PageState = (Maybe Int, JSString)

pureView :: PureReact
pureView = span_ "type here: "

leftView :: StatefulReact (Maybe Int) ()
leftView = div_ $ do
    i <- getState

    input_ <! value_ (iToS i)
           <! onChange (\_ evt -> sToI (targetValue evt))

rightView :: StatefulReact JSString ()
rightView = div_ $ do
    s <- getState

    input_ <! value_ s
           <! onChange (\_ evt -> targetValue evt)

statefulView :: StatefulReact PageState ()
statefulView = div_ $ do
    pureNest pureView
    nest leftLens leftView
    nest rightLens rightView

main :: IO ()
main = do
    Just elem <- elemById "inject"
    render (Just 1, "foo") elem statefulView
