{-# LANGUAGE OverloadedStrings #-}
module React.Interpret (reactNodeToJSAny, setProp') where

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as H
import Data.List
import Data.Maybe
import Data.Text (Text)

import React.GHCJS
import React.Imports
import React.Registry
import React.Types

-- This module handles the translation of 'ReactNode's into javascript. The
-- difficulty is in handling the complexity that arises from keeping state in
-- Haskell rather than in JS. This means we can't just blindly call ToJSRef
-- (which would be very convenient).
--
-- The raison d'etre of this module is 'reactNodeToJSAny', which delegates to
-- helpers for each type of react node.
--
-- Most interesting code is in 'componentToJSAny'.
-- * It deals with the current event handler, which is passed down through
--   'reactNodeToJSAny' and all of its special cases.
-- * It also sets the props and handler in the component registry.


data Attr = Attr Text JSON


reactNodeToJSAny :: (sig -> IO ()) -> Int -> ReactNode sig -> IO JSAny
reactNodeToJSAny sigHandler componentId (ComponentElement elem) =
    componentToJSAny sigHandler elem
reactNodeToJSAny sigHandler componentId (DomElement elem)       =
    domToJSAny sigHandler componentId elem

-- This isn't entirely fair to foreign (exported) classes. We've set them up
-- with our createClass machinery, so they expect to have a componentId, but we
-- give them none...
-- reactNodeToJSAny _ _ (ForeignElement elem)                   =
--     return $ castRef elem

reactNodeToJSAny sigHandler componentId (ForeignClass elem children) = do
    -- pass the handler and component id on to the children - their events will
    -- just be handled by the parent class.
    children' <- reactNodeToJSAny sigHandler componentId children
    js_foreignParent elem children'
    -- foreignClassToJSAny componentId cls

reactNodeToJSAny sigHandler _           (NodeText str)          =
    castRef <$> toJSRef str
reactNodeToJSAny sigHandler componentId (NodeSequence seq)      = do
    jsNodes <- mapM (reactNodeToJSAny sigHandler componentId) seq
    castRef <$> toArray jsNodes
reactNodeToJSAny sigHandler componentId (LocalNode f node)      =
    -- Convert from 'sig -> IO ()' to 'insig -> IO ()'
    let sigHandler' = sigHandler . f
    in reactNodeToJSAny sigHandler' componentId node


jsName :: EvtType -> JSString
jsName ChangeEvt = "onChange"
jsName KeyDownEvt = "onKeyDown"
jsName KeyPressEvt = "onKeyPress"
jsName KeyUpEvt = "onKeyUp"
jsName ClickEvt = "onClick"
jsName DoubleClickEvt = "onDoubleClick"
jsName MouseEnterEvt = "onMouseEnter"
jsName MouseLeaveEvt = "onMouseLeave"


unHandler :: (s -> IO ())
          -> EventHandler s
          -> (Int -> RawEvent -> Maybe (IO ()), EvtType)
unHandler act (EventHandler handle ty) = (\ix e -> act <$> handle ix e, ty)


makeHandler :: Int
            -- ^ component id
            -> JSAny
            -- ^ object to set this attribute on
            -> (Int -> RawEvent -> Maybe (IO ()), EvtType)
            -- ^ handler
            -> IO ()
makeHandler componentId obj (handle, evtTy) = do
    handle' <- handlerToJs handle
    js_set_handler componentId (jsName evtTy) handle' obj


-- | Make a javascript callback to synchronously execute the handler
handlerToJs :: (Int -> RawEvent -> Maybe (IO ()))
            -> IO (JSFun (JSRef Int -> RawEvent -> IO ()))
handlerToJs handle = syncCallback2 AlwaysRetain True $ \idRef evt -> do
    Just componentId <- fromJSRef idRef
    fromMaybe (return ()) (handle componentId evt)


attrsToJson :: [Attr] -> JSON
attrsToJson = Aeson.toJSON . H.fromList . map unAttr where
    unAttr (Attr name json) = (name, json)


separateAttrs :: [AttrOrHandler sig] -> ([Attr], [EventHandler sig])
separateAttrs attrHandlers = (map makeA as, map makeH hs) where
    (as, hs) = partition isAttr attrHandlers

    isAttr :: AttrOrHandler sig -> Bool
    isAttr (StaticAttr _ _) = True
    isAttr _ = False

    makeA :: AttrOrHandler sig -> Attr
    makeA (StaticAttr t j) = Attr t j

    makeH :: AttrOrHandler sig -> EventHandler sig
    makeH (Handler h) = h


attrHandlerToJSAny :: (sig -> IO ()) -> Int -> [AttrOrHandler sig] -> IO JSAny
attrHandlerToJSAny sigHandler componentId attrHandlers = do
    let (attrs, handlers) = separateAttrs attrHandlers
    starter <- castRef <$> toJSRef (attrsToJson attrs)

    forM_ handlers $ makeHandler componentId starter . unHandler sigHandler
    return starter


-- Helper for componentToJSAny and domToJSAny
setMaybeKey :: Maybe JSString -> JSAny -> IO ()
setMaybeKey maybeKey attrsObj = when (isJust maybeKey) $ do
    let Just key = maybeKey
    setProp' "key" key attrsObj


setProp' :: ToJSRef a => String -> a -> JSAny -> IO ()
setProp' key prop obj = do
    propRef <- toJSRef prop
    setProp key propRef obj


-- foreignElementToJSAny :: Int -> IO JSAny -> IO JSAny
-- foreignElementToJSAny componentId

-- foreignClassToJSAny :: Int -> ExportedClass -> IO JSAny
-- foreignClassToJSAny componentId cls = do


componentToJSAny :: (sig -> IO ()) -> ReactComponentElement sig -> IO JSAny
componentToJSAny
    sigHandler
    (ReactComponentElement ty children maybeKey ref props) = do

        let registry = classStateRegistry ty
        componentId <- allocProps registry props

        -- handle internal signals, maybe call external signal handler

        -- Register a handler! This transitions the class to its new state and
        -- outputs a signal if appropriate.
        let sigHandler' insig = do
                RegistryStuff _ state _ <-
                    lookupRegistry registry componentId
                let (state', maybeExSig) = classTransition ty (state, insig)
                setState registry state' componentId

                case maybeExSig of
                    Just exSig -> sigHandler exSig
                    Nothing -> return ()

        setHandler registry sigHandler' componentId

        attrsObj <- newObj

        setMaybeKey maybeKey attrsObj
        setProp' "ref" ref attrsObj
        setProp' "componentId" componentId attrsObj

        let ty' = classForeign ty
        children' <- reactNodeToJSAny sigHandler' componentId children

        castRef <$> js_react_createElement_Class ty' attrsObj children'


domToJSAny :: (sig -> IO ()) -> Int -> ReactDOMElement sig -> IO JSAny
domToJSAny sigHandler componentId (ReactDOMElement ty props children maybeKey ref) = do
    attrsObj <- attrHandlerToJSAny sigHandler componentId props

    setMaybeKey maybeKey attrsObj
    setProp' "ref" ref attrsObj

    children' <- reactNodeToJSAny sigHandler componentId children

    castRef <$> js_react_createElement_DOM ty attrsObj children'
