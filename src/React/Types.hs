{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, TypeFamilies,
    ExistentialQuantification, ImpredicativeTypes, LiberalTypeSynonyms,
    DeriveGeneric, DataKinds, GADTs, OverloadedStrings, CPP #-}

module React.Types where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.List (partition)
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)
import GHC.Generics
import System.IO.Unsafe

import qualified Data.Aeson as Aeson
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

instance Show JSString where
    show = fromJSString

type JSON = Aeson.Value

newtype ForeignNode = ForeignNode JSAny
newtype RawAttrs = RawAttrs JSAny
newtype ReactArray = ReactArray JSAny
newtype ForeignClass = ForeignClass JSAny
type ForeignRender = RawAttrs -> ReactArray -> IO ForeignNode

newtype RenderHandle = RenderHandle Int

data EvtType
   = ChangeEvt
   | KeyDownEvt
   | KeyPressEvt
   | KeyUpEvt
   | ClickEvt
   | DoubleClickEvt
   | MouseEnterEvt
   | MouseLeaveEvt


jsName :: EvtType -> JSString
jsName ChangeEvt = "onChange"
jsName KeyDownEvt = "onKeyDown"
jsName KeyPressEvt = "onKeyPress"
jsName KeyUpEvt = "onKeyUp"
jsName ClickEvt = "onClick"
jsName DoubleClickEvt = "onDoubleClick"
jsName MouseEnterEvt = "onMouseEnter"
jsName MouseLeaveEvt = "onMouseLeave"


data EventHandler signal = EventHandler
    { handler :: Int -> RawEvent -> Maybe signal
    , evtType :: EvtType
    }

data ReactType
    = RtClass
    | RtBuiltin
    | RtSequence


-- | A 'ReactClass' is a standalone component of a user interface which
-- contains the state necessary to render itself. Classes are a tool for
-- scoping.
--
-- Use 'createClass' to construct.
data ReactClass props state insig exsig = ReactClass
    { classForeign :: JSAny
    , classRender :: props -> state -> ReactNode insig
    , classInitialState :: state
    , className :: JSString
    , classTransition :: (state, insig) -> (state, Maybe exsig)

    , classStateRegistry :: ClassRegistry props state insig exsig
    }


data RegistryStuff props state insig exsig = RegistryStuff
    { registryProps :: props
    , registryState :: state
    , registryHandler :: (state, insig) -> (state, Maybe exsig)
    }


data ClassRegistry props state insig exsig = ClassRegistry
    { registryStuff :: IORef (H.HashMap Int (RegistryStuff props state insig exsig))
    , registryGen :: IORef Int
    }


generateKey :: ClassRegistry props state insig exsig -> IO Int
generateKey (ClassRegistry _ gen) = do
    k <- readIORef gen
    writeIORef gen (k + 1)
    return k


allocProps :: ClassRegistry props state insig exsig
           -> props
           -> ((state, insig) -> (state, Maybe exsig))
           -> IO Int
allocProps registry props handler = do
    k <- generateKey registry

    -- let handler' sig = do
    --         RegistryStuff _ prevState _ <- lookupRegistry registry k
    --         handler sig prevState

    modifyIORef (registryStuff registry) $
        H.insert k (RegistryStuff props undefined handler)
    return k


setState :: ClassRegistry props state insig exsig -> state -> Int -> IO ()
setState registry state k =
    modifyIORef (registryStuff registry) $
        H.adjust (\(RegistryStuff p _ h) -> RegistryStuff p state h) k


deallocRegistry :: ClassRegistry props state insig exsig -> Int -> IO ()
deallocRegistry (ClassRegistry stuff _) k = modifyIORef stuff (H.delete k)


-- TODO(joel) - think about pushing around the IO boundary
lookupRegistry :: ClassRegistry props state insig exsig
               -> Int
               -> IO (RegistryStuff props state insig exsig)
lookupRegistry (ClassRegistry stuff _) k = do
    stuff' <- readIORef stuff
    return $ H.lookupDefault
        (error $ "class registry didn't contain an entry!\n" ++
                 "componentId: " ++ show k ++ "\n" ++
                 "keys: " ++ show (H.keys stuff') ++ "\n"
        )
        k
        stuff'


-- TODO use phantom type to indicate renderability? Only sequence is not.
data ReactNode sig
    = ComponentElement (ReactComponentElement sig)
    | DomElement (ReactDOMElement sig)
    | NodeText JSString
    | NodeSequence [ReactNode sig]


instance Monoid (ReactNode sig) where
    mempty = NodeSequence []

    (NodeSequence xs) `mappend` (NodeSequence ys) = NodeSequence (xs <> ys)
    (NodeSequence xs) `mappend` y = NodeSequence (xs <> [y])
    x `mappend` (NodeSequence ys) = NodeSequence (x : ys)
    x `mappend` y = NodeSequence [x, y]


reactNodeToJSAny :: (sig -> IO ()) -> Int -> ReactNode sig -> IO JSAny
reactNodeToJSAny sigHandler componentId (ComponentElement elem) =
    componentToJSAny sigHandler elem
reactNodeToJSAny sigHandler componentId (DomElement elem)       =
    domToJSAny sigHandler componentId elem
reactNodeToJSAny sigHandler _           (NodeText str)          =
    castRef <$> toJSRef str
reactNodeToJSAny sigHandler componentId (NodeSequence seq)      = do
    jsNodes <- mapM (reactNodeToJSAny sigHandler componentId) seq
    castRef <$> toArray jsNodes


instance IsString (ReactNode sig) where
    fromString str = NodeText (fromString str)


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


data AttrOrHandler signal
    = StaticAttr Text JSON
    | Handler (EventHandler signal)

data Attr = Attr Text JSON


data ReactComponentElement exsig = forall props state insig. ReactComponentElement
    { reComType :: ReactClass props state insig exsig
    , reComAttrs :: [AttrOrHandler insig]
    , reComChildren :: ReactNode insig
    , reComKey :: Maybe JSString
    , reComRef :: Maybe JSString

    -- Props are stored here, not used until, `render` because we need both the
    -- props and state at the same time.
    , reComProps :: props

    -- We can't store the class id here because we don't know it until *after*
    -- render has run! It's not allocated until componentWillMount.
    -- , reComClassId :: Int
    }


componentToJSAny :: (sig -> IO ()) -> ReactComponentElement sig -> IO JSAny
componentToJSAny
    sigHandler
    (ReactComponentElement ty attrs children maybeKey ref props) = do

        componentId <- allocProps (classStateRegistry ty) props (classTransition ty)

        -- handle internal signals, maybe call external signal handler
        let sigHandler' insig = do
                putStrLn "in sigHandler'"
                stuff <- lookupRegistry (classStateRegistry ty) componentId
                let prevState = registryState stuff
                    (newState, maybeExSig) = classTransition ty (prevState, insig)

                case maybeExSig of
                    Just exSig -> sigHandler exSig
                    Nothing -> return ()

        attrsObj <- attrHandlerToJSAny sigHandler' componentId attrs

        when (isJust maybeKey) $ do
            let Just key = maybeKey
            keyProp <- toJSRef key
            setProp ("key" :: String) keyProp attrsObj

        refProp <- toJSRef ref
        setProp ("ref" :: String) refProp attrsObj

        idProp <- toJSRef componentId
        setProp ("componentId" :: String) idProp attrsObj

        let ty' = classForeign ty
        children' <- reactNodeToJSAny sigHandler' componentId children

        castRef <$> js_react_createElement_Class ty' attrsObj children'


data ReactDOMElement sig = ReactDOMElement
    { reDomType :: JSString
    , reDomProps :: [AttrOrHandler sig]
    , reDomChildren :: ReactNode sig
    , reDomKey :: Maybe JSString
    , reDomRef :: Maybe JSString
    }


domToJSAny :: (sig -> IO ()) -> Int -> ReactDOMElement sig -> IO JSAny
domToJSAny sigHandler componentId (ReactDOMElement ty props children maybeKey ref) = do
    attrsObj <- attrHandlerToJSAny sigHandler componentId props

    when (isJust maybeKey) $ do
        let Just key = maybeKey
        keyProp <- toJSRef key
        setProp ("key" :: String) keyProp attrsObj

    refProp <- toJSRef ref
    setProp ("ref" :: String) refProp attrsObj

    children' <- reactNodeToJSAny sigHandler componentId children

    castRef <$> js_react_createElement_DOM ty attrsObj children'


-- attributes

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
    case handle componentId evt of
        Nothing -> return ()
        Just x -> x


mkStaticAttr :: Aeson.ToJSON a => Text -> a -> AttrOrHandler sig
mkStaticAttr name = StaticAttr name . Aeson.toJSON


mkEventHandler :: (FromJSRef evt, NFData evt)
               => EvtType
               -> (evt -> Maybe signal)
               -> AttrOrHandler signal
mkEventHandler ty handle =
    -- XXX unsafe as fuck
    -- XXX throwing away ix
    let handle' ix raw = case unsafePerformIO $ fromJSRef $ castRef raw of
            Just x -> handle $!! x
            Nothing -> Nothing
    -- let handle' raw = handle $!! fromJust $ unsafePerformIO $ fromJSRef $ castRef raw
    in Handler (EventHandler handle' ty)


-- | Low level properties common to all events
data EventProperties e =
  EventProperties { bubbles :: !Bool
                  , cancelable :: !Bool
                  , currentTarget :: !e -- NativeElem
                  , defaultPrevented :: !Bool
                  , eventPhase :: !Int
                  , isTrusted :: !Bool
                    -- ,  nativeEvent :: DOMEvent
                    -- , preventDefault :: IO ()
                    -- ,  stopPropagation :: IO ()
                  , evtTarget :: !e -- NativeElem
                    --, timeStamp :: Date
                  , eventType :: !JSString -- type
                  }

instance NFData e => NFData (EventProperties e) where
    rnf (EventProperties a b c d e f g h) =
        a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` g `seq` h `seq` ()

data ModifierKeys =
  ModifierKeys { altKey :: !Bool
               , ctrlKey :: !Bool
               , metaKey :: !Bool
               , shiftKey :: !Bool
               } deriving (Eq, Show, Generic)

instance FromJSRef ModifierKeys where

instance NFData ModifierKeys where
    rnf (ModifierKeys a b c d) = a `seq` b `seq` c `seq` d `seq` ()

data MouseEvent =
  MouseEvent { -- mouseEventProperties :: !(EventProperties e)
               -- mouseModifierKeys :: !ModifierKeys
             -- , buttonNum :: !Int -- "button"
               -- , buttons :: Int
               clientX :: !Double
             , clientY :: !Double
             , pageX :: !Double
             , pageY :: !Double
               -- , relatedTarget :: Unpacked
             , screenX :: !Double
             , screenY :: !Double
             } deriving (Show, Generic)

instance FromJSRef MouseEvent where

instance NFData MouseEvent where
    -- rnf (MouseEvent a b c d e f g h) =
    --     a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` g `seq` h `seq` ()
    rnf (MouseEvent a b c d e f) =
        a `seq` b `seq` c `seq` d `seq` e `seq` f  `seq` ()

data KeyboardEvent =
  KeyboardEvent { -- keyboardEventProperties :: ! (EventProperties e)
                -- keyboardModifierKeys :: !ModifierKeys
                  charCode :: !Int
                , key :: !JSString
                , keyCode :: !Int
                -- , locale :: !JSString
                , location :: !Int
                , repeat :: !Bool
                , which :: !Int
                } deriving (Show, Generic)

instance FromJSRef KeyboardEvent where

instance NFData KeyboardEvent where
    rnf (KeyboardEvent a b c d e f) =
        a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` ()

data Target = Target
    { value :: !JSString
    , tagName :: !JSString
    -- XXX(joel) This is gross. Added a second field so that the generic
    -- FromJSRef instance does the right thing. Without a second field it
    -- uses the FromJSRef instance for `value`.
    } deriving (Show, Generic)

instance FromJSRef Target where

data ChangeEvent = ChangeEvent
    { target :: !Target
    , timeStamp :: !Int
    } deriving (Show, Generic)

instance FromJSRef ChangeEvent where

instance NFData ChangeEvent where
    rnf e@(ChangeEvent str stamp) = str `seq` stamp `seq` ()

data FocusEvent e =
  FocusEvent { -- focusEventProperties :: ! (EventProperties e)
               domEventTarget :: !e -- NativeElem
             , relatedTarget :: !e -- NativeElem
             }

instance NFData e => NFData (FocusEvent e) where
    rnf (FocusEvent a b) = a `seq` b `seq` ()
