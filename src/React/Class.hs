{-# LANGUAGE NamedFieldPuns, OverloadedStrings, DataKinds,
    ExistentialQuantification, ConstraintKinds #-}
module React.Class
    ( ReactClass(..)
    , ClassConfig(..)
    , ClassCtx
    , PropRequired(..)
    , PropType(..)
    , createClass
    , smartClass
    , dumbClass
    ) where


import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import System.IO.Unsafe

import React.Imports
import React.Interpret
import React.Registry
import React.Types
import React.PropTypes


data ClassConfig props state insig exsig ctx = ClassConfig
    { renderFn :: props -> state -> ReactNode insig
    , initialState :: state
    , name :: JSString
    , transition :: (state, insig) -> (state, Maybe exsig)
    , startupSignals :: [insig]

    -- lifecycle methods!
    -- TODO(joel) - Lifcycle monad!

--     , componentWillMount :: props -> state -> IO state
--     , componentDidMount
--     , componentWillReceiveProps
--     , shouldComponentUpdate
--     , componentWillUpdate
--     , componentDidUpdate
--     , componentWillUnmount

    -- TODO(joel) - this is static for now - should it be dynamic? and what
    -- does that dynamic implementation have access to?
    , childContext :: Maybe (H.HashMap Text ctx)
    }


dumbClass :: ClassConfig props () sig sig JSString
dumbClass = ClassConfig
    { name = "Anonymous Stateless Class"
    , renderFn = \_ _ -> "give this class a `render`!"
    , initialState = ()
    , transition = \(state, sig) -> (state, Just sig)
    , startupSignals = []
    , childContext = Nothing
    }


smartClass :: ClassConfig props state insig exsig JSString
smartClass = ClassConfig
    { name = "Anonymous Stateful Class"
    , renderFn = \_ _ -> "give this class a `render`!"
    , initialState = error "must define `initialState`!"
    , transition = error "must define `transition`!"
    , startupSignals = []
    , childContext = Nothing
    }


willMount :: ClassRegistry props state insig exsig -> state -> JSRef Int -> IO ()
willMount registry state idRef = do
    -- initialize state in registry
    Just componentId <- fromJSRef idRef
    setState registry state componentId


willUnmount :: ClassRegistry props state insig exsig -> JSRef Int -> IO ()
willUnmount registry idRef = do
    -- remove state from registry
    Just componentId <- fromJSRef idRef
    deallocRegistry registry componentId


type ClassCtx a = (ToJSRef a, PropTypable a)


-- TODO(joel) why not just pass in the class config?
render :: ClassRegistry props state insig exsig
       -> (props -> state -> ReactNode insig)
       -> JSRef (Int, JSAny)
       -> JSAny
       -> IO ()
render registry renderFn inRefs returnObj = do
    -- The fundamental tension here is that this render function is
    -- defined for the class, but each invocation needs access to a
    -- specific instance. How do we get a handle to that specific
    -- instance? This will give us the props and state (and
    -- handler?)

    Just (componentId, thisObj) <- fromJSRef inRefs
    RegistryStuff thisProps thisState thisHandler <-
        lookupRegistry registry componentId

    let rendered = renderFn thisProps thisState

        -- We need to forceUpdate here because we have access to `this`. But
        -- the registered handler does most of the work.
        handler sig = do
            js_forceUpdate thisObj
            thisHandler sig
    ret <- reactNodeToJSAny handler componentId rendered
    setProp ("value" :: JSString) ret returnObj


createClass :: ClassCtx ctx
            => ClassConfig props state insig exsig ctx
            -> ReactClass props state insig exsig ctx
createClass ClassConfig{renderFn,
                        initialState,
                        name,
                        transition,
                        startupSignals,
                        childContext} =

    -- TODO(joel) - verify this use of unsafePerformIO is, well, safe
    let classRegistry = unsafePerformIO $ ClassRegistry
            <$> newIORef H.empty
            <*> newIORef 0

        foreignObj = do
            obj <- newObj

            setProp ("displayName" :: JSString) name obj

            renderCb <- syncCallback2 NeverRetain True
                (render classRegistry renderFn)
            setProp ("render" :: JSString) renderCb obj

            when (isJust childContext) $ do
                let childContext' = fromJust childContext

                ctxObj <- newObj
                ctxTypeObj <- newObj
                forM_ (H.toList childContext') $ \(k, v) -> do
                    ref <- toJSRef v
                    setProp k ref ctxObj

                    let ty = toJsPropType (propType v)
                    setProp k ty ctxTypeObj

                setProp' "childContext" ctxObj obj
                setProp' "childContextTypes" ctxTypeObj obj

            willMountCb <- syncCallback1 NeverRetain True
                (willMount classRegistry initialState)
            setProp ("componentWillMount" :: JSString) willMountCb obj

            willUnmountCb <- syncCallback1 NeverRetain True
                (willUnmount classRegistry)
            setProp ("componentWillUnmount" :: JSString) willUnmountCb obj

            return obj

        foreignClass = unsafePerformIO $ js_createClass <$> foreignObj

    in ReactClass foreignClass transition classRegistry
