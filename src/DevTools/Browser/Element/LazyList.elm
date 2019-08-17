module DevTools.Browser.Element.LazyList exposing
    ( Model
    , Msg
    , decoder
    , encode
    , init
    , update
    , view
    )

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Model =
    { fromIndex : Int
    , toIndex : Int
    }


type Msg
    = ViewFrom Int


init : Int -> Model
init toIndex =
    { fromIndex = 0
    , toIndex = toIndex
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ViewFrom fromIndex ->
            { fromIndex = fromIndex
            , toIndex = fromIndex + (model.toIndex - model.fromIndex)
            }


view :
    Model
    ->
        { viewElement : a -> Html msg
        , queryElements : Int -> Int -> List a
        , length : Int
        , elementHeight : Int
        , onUpdate : Msg -> msg
        }
    -> Html msg
view model config =
    div
        [ style "height" "100%"
        , style "overflow" "scroll"
        , on "scroll"
            (Decode.at [ "target", "scrollTop" ]
                (Decode.map
                    (\pxFromTop ->
                        config.onUpdate (ViewFrom (pxFromTop // config.elementHeight))
                    )
                    Decode.int
                )
            )
        ]
        [ div [ style "height" (String.fromInt (config.elementHeight * config.length) ++ "px") ]
            [ div [ style "padding-top" (String.fromInt (config.elementHeight * model.fromIndex) ++ "px") ]
                (List.map config.viewElement
                    (config.queryElements model.fromIndex (model.toIndex + 1))
                )
            ]
        ]


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
