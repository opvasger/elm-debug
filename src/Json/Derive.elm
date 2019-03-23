module Json.Derive exposing
    ( customTypeToDecoder
    , customTypeToEncoder
    , typeAliasToDecoder
    , typeAliasToEncoder
    )

import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)


typeAliasToEncoder : TypeAlias -> Function
typeAliasToEncoder typeAlias =
    Debug.todo "not implemented yet"


customTypeToEncoder : Type -> Function
customTypeToEncoder customType =
    Debug.todo "not implemented yet"


typeAliasToDecoder : TypeAlias -> Function
typeAliasToDecoder typeAlias =
    Debug.todo "not implemented yet"


customTypeToDecoder : Type -> Function
customTypeToDecoder customType =
    Debug.todo "not implemented yet"
