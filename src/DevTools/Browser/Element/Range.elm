module DevTools.Browser.Element.Range exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Html exposing (Html, div)
import Html.Attributes exposing (style, title)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Decode exposing (Decoder)


type Msg
    = ToggleMouseOver


type alias Model =
    { isMouseOver : Bool
    }


init : Model
init =
    { isMouseOver = False
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleMouseOver ->
            { model | isMouseOver = not model.isMouseOver }


subscriptions :
    { config
        | onMove : Int -> msg
        , value : Int
        , max : Int
    }
    -> Sub msg
subscriptions config =
    Sub.map config.onMove
        (Browser.Events.onKeyDown (keyPressDecoder config))


view :
    Model
    ->
        { config
            | onUpdate : Msg -> msg
            , onMove : Int -> msg
            , title : String
            , max : Int
            , value : Int
        }
    -> Html msg
view model config =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "align-items" "center"
        , style "flex-grow" "1"
        , style "height" "15px"
        , style "cursor" "pointer"
        , title config.title
        , stopPropagationOn "click" (clickDecoder config)
        , stopPropagationOn "mouseover" (Decode.succeed ( config.onUpdate ToggleMouseOver, True ))
        , stopPropagationOn "mouseout" (Decode.succeed ( config.onUpdate ToggleMouseOver, True ))
        ]
        [ div
            [ style "width" "100%"
            , style "margin" "0px 2px"
            , style "height" "2px"
            , style "pointer-events" "none"
            , if model.isMouseOver then
                style "background-color" "black"

              else
                style "background-color" "#6e6e6e"
            ]
            [ div
                [ style "width"
                    (String.fromFloat
                        (toFloat config.value
                            * 100
                            / toFloat config.max
                        )
                        ++ "%"
                    )
                , style "pointer-events" "none"
                , style "transition" "width .2s"
                , style "height" "2px"
                , style "background-color" "#89b3ee"
                ]
                []
            ]
        ]


clickDecoder :
    { config
        | onMove : Int -> msg
        , max : Int
    }
    -> Decoder ( msg, Bool )
clickDecoder config =
    Decode.map2
        (\max x ->
            ( x
                / max
                * toFloat config.max
                |> round
                |> config.onMove
            , True
            )
        )
        (Decode.at [ "target", "offsetWidth" ] Decode.float)
        (Decode.field "offsetX" Decode.float)


keyPressDecoder :
    { config
        | value : Int
        , max : Int
    }
    -> Decoder Int
keyPressDecoder config =
    Decode.andThen
        (\keyCode ->
            case keyCode of
                37 ->
                    Decode.succeed (clamp 0 config.max (config.value - 1))

                39 ->
                    Decode.succeed (clamp 0 config.max (config.value + 1))

                _ ->
                    Decode.fail ""
        )
        (Decode.field "keyCode" Decode.int)
