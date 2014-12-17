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

--

type PageState = (Maybe Int, JSString)

initialState :: PageState
initialState = (Just 1, "foo")

--

data LeftTransition = Number (Maybe Int)
data RightTransition = Typing JSString

data Transition
    = LeftT (Maybe Int)
    | RightT JSString

transition :: PageState -> Transition -> PageState
transition (n, s) (LeftT n') = (n', s)
transition (n, s) (RightT s') = (n, s')

generalizeLeft :: LeftTransition -> Transition
generalizeLeft (Number x) = LeftT x

generalizeRight :: RightTransition -> Transition
generalizeRight (Typing x) = RightT x

--

pureView :: PureReact
pureView = span_ "type here: "

leftView :: Maybe Int -> React LeftTransition ()
leftView i = div_ $
    input_ <! value_ (iToS i)
           <! onChange (Just . Number . sToI . targetValue)

rightView :: JSString -> React RightTransition ()
rightView s = div_ $
    input_ <! value_ s
           <! onChange (Just . Typing . targetValue)

view :: PageState -> React Transition ()
view (n, s) = div_ $ do
    -- pureNest pureView
    locally generalizeLeft (leftView n)
    locally generalizeRight (rightView s)

main :: IO ()
main = do
    Just elem <- elemById "inject"
    render elem view transition initialState
