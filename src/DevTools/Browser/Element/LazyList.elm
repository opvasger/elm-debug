module DevTools.Browser.Element.LazyList exposing
    ( Model
    , Msg
    , decoder
    , encode
    , init
    , view
    )

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Model =
    { fromIndex : Int
    , toIndex : Int
    }


type Msg
    = ScrollTo Int


init : Int -> Model
init toIndex =
    { fromIndex = 0
    , toIndex = toIndex
    }


view :
    Model
    ->
        { viewElement : a -> Html msg
        , queryElements : Int -> Int -> List a
        , length : Int
        , elementHeight : Int
        }
    -> Html msg
view _ config =
    div
        [ style "padding" "4px"
        ]
        (List.map config.viewElement (config.queryElements (config.length - 9) config.length))


decoder : Decoder Model
decoder =
    Decode.map2
        (\fromIndex toIndex ->
            { fromIndex = fromIndex
            , toIndex = toIndex
            }
        )
        (Decode.field "fromIndex" Decode.int)
        (Decode.field "toIndex" Decode.int)


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "fromIndex", Encode.int model.fromIndex )
        , ( "toIndex", Encode.int model.toIndex )
        ]
