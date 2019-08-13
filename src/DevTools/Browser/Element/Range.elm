module DevTools.Browser.Element.Range exposing
    ( subscriptions
    , view
    )

import Browser.Events
import Html exposing (Html, div, text)
import Html.Attributes exposing (style, title)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Decode exposing (Decoder)


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
    { config
        | onMove : Int -> msg
        , title : String
        , max : Int
        , value : Int
    }
    -> Html msg
view config =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "flex-grow" "1"
        , style "height" "15px"
        , style "cursor" "pointer"
        , title config.title
        , stopPropagationOn "click" (clickDecoder config)
        ]
        [ div
            [ style "font-size" "8px"
            , style "color" "#6e6e6e"
            , style "margin" "-4px 0"
            , style "font-family" "monospace"
            , style "font-weight" "bold"
            , style "left" "0px"
            , style "align-self" "flex-end"
            , style "transition" "margin-right .2s ease-in"
            , style "margin-right"
                (String.fromFloat
                    (97
                        - toFloat config.value
                        * 97
                        / toFloat config.max
                    )
                    ++ "%"
                )
            ]
            [ text (String.fromInt config.value) ]
        , div
            [ style "width" "100%"
            , style "margin-top" "4.5px"
            , style "height" "4px"
            , style "border-radius" "5px"
            , style "pointer-events" "none"
            , style "background-color" "#6e6e6e"
            , style "background-image" "linear-gradient(to top, rgba(0,0,0,.3), rgba(0,0,0,0))"
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
                , style "transition" "width .2s ease-in"
                , style "height" "4px"
                , style "border-radius" "5px"
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
