{-# LANGUAGE CPP, FlexibleInstances #-}

#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#else
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
#endif

module React.PropTypes where

import Data.Monoid
import Data.Text (Text)

import React.GHCJS
import React.Imports


data FPropType_
type FPropType = JSRef FPropType_

#ifdef __GHCJS__
foreign import javascript unsafe "React.PropTypes.bool"
    fPropBool :: FPropType
foreign import javascript unsafe "React.PropTypes.func"
    fPropFunc :: FPropType
foreign import javascript unsafe "React.PropTypes.number"
    fPropNumber :: FPropType
foreign import javascript unsafe "React.PropTypes.string"
    fPropString :: FPropType
foreign import javascript unsafe "React.PropTypes.object"
    fPropObject :: FPropType
foreign import javascript unsafe "$1.isRequired"
    fIsRequired :: FPropType -> FPropType
#else
fPropBool :: FPropType
fPropBool = undefined
fPropFunc :: FPropType
fPropFunc = undefined
fPropNumber :: FPropType
fPropNumber = undefined
fPropString :: FPropType
fPropString = undefined
fPropObject :: FPropType
fPropObject = undefined
fIsRequired :: FPropType -> FPropType
fIsRequired = undefined
#endif


data PropRequired = IsRequired | IsntRequired


data PropType
    -- = PropArrayOf PropType
    = PropBool PropRequired
    | PropFunc PropRequired
    | PropNumber PropRequired
    | PropString PropRequired
    | PropObject PropRequired
    -- PropArray
    -- PropShape (H.HashMap Text PropType)
    -- PropEnum [Text]
    -- PropUnion [PropType]


toJsPropType :: PropType -> FPropType
toJsPropType (PropBool req)   = ptReq req fPropBool
toJsPropType (PropFunc req)   = ptReq req fPropFunc
toJsPropType (PropNumber req) = ptReq req fPropNumber
toJsPropType (PropString req) = ptReq req fPropString
toJsPropType (PropObject req) = ptReq req fPropObject

ptReq :: PropRequired -> FPropType -> FPropType
ptReq IsRequired = fIsRequired
ptReq IsntRequired = id

class PropTypable a where
    propType :: a -> PropType


instance PropTypable (JSRef ()) where
    -- TOOD(joel) instanceOf this
    propType _ = PropObject IsRequired

instance PropTypable JSString where
    propType _ = PropString IsRequired

