module Elm.Syntax.Json exposing
    ( decoderForCustomType
    , decoderForTypeAlias
    , encoderForCustomType
    , encoderForTypeAlias
    )

import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)



-- Type Alias


encoderForTypeAlias : TypeAlias -> Function
encoderForTypeAlias _ =
    Debug.todo "not implemented yet"


decoderForTypeAlias : TypeAlias -> Function
decoderForTypeAlias _ =
    Debug.todo "not implemented yet"



-- Custom Type


encoderForCustomType : Type -> Function
encoderForCustomType _ =
    Debug.todo "not implemented yet"


decoderForCustomType : Type -> Function
decoderForCustomType _ =
    Debug.todo "not implemented yet"
