module Input.Range exposing
    ( Config
    , Model
    , init
    , subscriptions
    , view
    )

import Browser.Events
import Help
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Model
    = Inactive
    | MovingThumb Int


init : Model
init =
    Inactive


type alias Config msg =
    { updateMsg : Model -> msg
    , inputMsg : Int -> msg
    , model : Model
    , value : Int
    , maxValue : Int
    }


subscriptions : Config msg -> Sub msg
subscriptions config =
    case config.model of
        Inactive ->
            Sub.none

        MovingThumb fromX ->
            Sub.batch
                [ Browser.Events.onMouseUp
                    (Decode.succeed (config.updateMsg Inactive))
                , Browser.Events.onMouseMove
                    (Decode.map config.inputMsg
                        (moveEventDecoder fromX config.value config.maxValue)
                    )
                ]


view : Config msg -> Html msg
view { value, maxValue, inputMsg, updateMsg, model } =
    let
        pctFromLeft =
            Basics.max 0 (toFloat value * 100 / toFloat maxValue)
    in
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , if model /= Inactive then
            style "cursor" "grabbing"

          else if maxValue > 0 then
            style "cursor" "pointer"

          else
            style "" ""
        , if maxValue > 0 then
            on "click" (Decode.map inputMsg (clickEventDecoder maxValue))

          else
            style "" ""
        ]
        [ div
            [ style "background-color" Help.mutedGray
            , style "width" "140px"
            , style "margin" "0 7px"
            , style "height" "2px"
            , if model /= Inactive then
                style "pointer-events" "none"

              else
                style "" ""
            ]
            [ div
                [ style "width" (String.fromFloat pctFromLeft ++ "%")
                , style "background-color" Help.activeBlue
                , style "height" "2px"
                , style "transition" "width .2s"
                ]
                []
            , div
                [ style "height" "12px"
                , style "width" "12px"
                , style "border-radius" "15px"
                , style "margin-left" ("calc(" ++ String.fromFloat pctFromLeft ++ "% - 7px)")
                , style "margin-top" "-7px"
                , style "transition" "background-color .2s, box-shadow .2s, margin-left .2s"
                , title ("no. " ++ String.fromInt (value + 1) ++ " of " ++ String.fromInt (maxValue + 1) ++ " states")
                , if model /= Inactive then
                    style "" ""

                  else if maxValue > 0 then
                    style "cursor" "grab"

                  else
                    style "" ""
                , if maxValue > 0 then
                    on "mousedown" (Decode.map updateMsg startMoveEventDecoder)

                  else
                    style "" ""
                , if maxValue < 1 then
                    style "background-color" Help.backgroundGray

                  else
                    style "background-color" Help.activeBlue
                , if maxValue < 1 then
                    style "box-shadow" ("0px 0px 0px 1.5px " ++ Help.mutedGray ++ " inset")

                  else
                    style "box-shadow" "none"
                ]
                []
            ]
        ]


startMoveEventDecoder : Decoder Model
startMoveEventDecoder =
    Decode.map MovingThumb (Decode.field "offsetX" Decode.int)


clickEventDecoder : Int -> Decoder Int
clickEventDecoder maxValue =
    Decode.map2
        (\max x -> round (toFloat x * toFloat maxValue / toFloat max))
        (Decode.at [ "target", "offsetWidth" ] Decode.int)
        (Decode.field "offsetX" Decode.int)


moveEventDecoder : Int -> Int -> Int -> Decoder Int
moveEventDecoder initX value maxValue =
    Decode.andThen
        (\nextValue ->
            if nextValue == value then
                Decode.fail ""

            else
                Decode.succeed nextValue
        )
        (Decode.map2
            (\max x ->
                toFloat x
                    / toFloat max
                    |> (*) (toFloat maxValue)
                    |> round
            )
            (Decode.at [ "target", "offsetWidth" ] Decode.int)
            (Decode.field "offsetX" Decode.int)
        )
