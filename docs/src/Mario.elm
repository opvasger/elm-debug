module Mario exposing
    ( encodeModel
    , encodeMsg
    , init
    , msgDecoder
    , subscriptions
    , update
    , view
    )

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import Task


type alias Velocity =
    { vertical : Float
    , horizontal : Float
    }


type alias Size =
    { width : Int
    , height : Int
    }


type alias Position =
    { left : Float
    , top : Float
    }


type Direction
    = Vertical VerticalDirection
    | Horizontal HorizontalDirection


type HorizontalDirection
    = Left
    | Right


type VerticalDirection
    = Up
    | Down


type alias Controls =
    { up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    }


type Msg
    = NextFrame Float
    | WindowResize Int Int
    | Press Direction
    | Release Direction


type alias Model =
    { face : HorizontalDirection
    , position : Position
    , velocity : Velocity
    , size : Size
    , controls : Controls
    }


init : ( Model, Cmd Msg )
init =
    ( { face = Right
      , position = Position 0 0
      , velocity = Velocity 0 0
      , size = Size 0 0
      , controls = Controls False False False False
      }
    , Task.perform fromViewport Browser.Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextFrame latency ->
            let
                adjustedLatency =
                    latency / 10
            in
            ( model
                |> gravity adjustedLatency
                |> jump
                |> walk
                |> physics adjustedLatency
            , Cmd.none
            )

        WindowResize width height ->
            ( physics 1
                { model
                    | size = Size width height
                }
            , Cmd.none
            )

        Press dir ->
            ( { model
                | controls = updateControls True dir model.controls
              }
            , Cmd.none
            )

        Release dir ->
            ( { model
                | controls = updateControls False dir model.controls
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta NextFrame
        , Browser.Events.onResize WindowResize
        , Browser.Events.onKeyDown (Decode.map Press (Decode.andThen toDirectionDecoder (Decode.field "key" Decode.string)))
        , Browser.Events.onKeyUp (Decode.map Release (Decode.andThen toDirectionDecoder (Decode.field "key" Decode.string)))
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Example"
    , body =
        [ viewMario model
        ]
    }


viewMario : Model -> Html Msg
viewMario { face, position, velocity, size } =
    Html.img
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" (toPx position.left)
        , Html.Attributes.style "top" (toPx (toFloat size.height - 35 - position.top))
        , Html.Attributes.src (toImageSrc position velocity face)
        , Html.Attributes.alt "Mario"
        ]
        []


toImageSrc : Position -> Velocity -> HorizontalDirection -> String
toImageSrc { top } { horizontal } face =
    "data:image/png;base64, "
        ++ (case ( face, top > 0, horizontal /= 0 ) of
                ( Left, True, _ ) ->
                    "R0lGODlhIwAjAPMAAAAAABIJBC4NAHYAAlEtEwMlYrwAA6tnOvj2L9iNZRBFoRSX25KOhv2yiMbHtfT15yH5BAUAABAALAAAAAAjACMAAAXMICSOZGmeaKqubOu+cCzPdAvcd40Cg+MMg5yOBHgYDEYgYCjCOY/BIeBIPd4M0RoPyB1YsUttz5HwKcHSx6KxeCQSAkFW+wAkig244B2e2RuACYBxAnl9MnZvbwGDiocxAZGSkXlwjzKTjJUCl4g+iglcnTAADg8/XFBziAwPrg5VUKM2DKansUo0OLUODFVABbMqAAcEBQgKCkBYwMF+B8XGyVwF1cLD0ATG1dzV2tcpxMVO3d+60Ic32gQH4OGd6u1MO+7z9vf4+SQhADs="

                ( Left, _, True ) ->
                    "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"

                ( Left, _, _ ) ->
                    "R0lGODlhIwAjAPQAAAAAABIJBC4NAHYAAlEtEwMlYrwAA6tnOvj2L9iNZRBFoRSX25KOhv2yiMbHtfT15//4/wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAkAABAAIf8LTkVUU0NBUEUyLjADAQAAACwAAAAAIwAjAAAFzyAkjmRpnmiqrmzrvnAMzHPMAoPjDENto4CHwSDkAX4mmnLYQ44Aw+hwZmg6cbzsYFo9XnOOhM7YdYqCi8bikUgIBNbrA5AINtyCtvdHb/gTfm8Cd3s2dG1tAYGIhTEBj5CPd26NNpGKkwKVfIidm3wFPAWhRmZnBUMKoUxxfAqpq1Gthgq1qjyyny4ACLW9qzoMui0ABbXGCjoPDw7DxLa1DsvMzjfGyMnMwqYQxcijDNvc3qPl1cQE6QDp6uMH7wDv8ucrSvY+3Pn6+/xOIQA7"

                ( Right, True, _ ) ->
                    "R0lGODlhIwAjAPMQAAAAABIJBC4NAHYAAlEtEwMlYrwAA6tnOvj2L9iNZRBFoRSX25KOhv2yiMbHtfT15yH5BAUAABAALAAAAAAjACMAAAXIICSOZGmeaKqubOu+cCzPdAzcd33ew+A4A4COBOgZHoajcAgpJnHQofOWrD5r02Rv21vOpr1f4hfUFQWCROKxaCwe3q8anWjAEwC4uY4WNBp1f3hYagl+DQGFanEyAGl1AZGSkVKPf5MBTEU9ij+MjUYGXA4PDp8wTlYGpA8PDKcum6qsDq9YBaFJDD+vOV8FuEY9CgoIBQQHsCwAwMEDxMfIyV8EBM3X0QfTjdXXUNLKKwAH1QS+IuPbjeTnROHL70zy8/T1NCEAOw=="

                ( Right, _, True ) ->
                    "R0lGODlhIwAjAPQQAAAAABIJBC4NAHYAAlEtEwMlYrwAA6tnOvj2L9iNZRBFoRSX25KOhv2yiMbHtfT15//4/wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh+QQJAAAQACwAAAAAIwAjAAAF1iAkjmRpnmiqrmzrvnBMAjQtq/QwOM4A3CeAzvAwEH/A2cBYayZFQgPNSGUmowCjbqtD3rA6XoLnewoFgkTisWgsHt6vGp1owBMAuLmOFjQadX94V2oJfg0BhWpxMgBpdQGRkpFPEI5qf5MBlVCKhYxAQjoFBaOgjUMGAwUKRgWnMFFUq60GCrAusqq0Cr24LQAMPKqsCL0IvywADg8PPL2sCq9mzM0OvdjJysLO2KzTlcEMpNDgnACk6aTauQTuBADv7MoH9fUA9vM4Tfz6nP8AAwo0EQIAIfkECQAAEAAsCgAFABAAGgAABbMgJI4QYJpkag6D4wxAKgKs8Rh2rA746ZM0gwlH7M14ABxrydIFaS1HwgUb0QSCROKxaCweupIWm2iAEwCwtYwVNBrlN3qmTbgbgbo2DMiWA4CBgEB+b4IBMgB6dWFWSwUFLAWNJTUGAwUKOJNAPESYmgYKfJ4GLi4KqY1BDg+uDqkIqzytr6mcVjQMrbC3uAAEkAW7k8IEBDHAwZAoygQHyQfHyFbPB9Az0sdO1yi5J7kjIQAh+QQJAAAQACwKAAYAEQAZAAAFqSAkjhBgmmRaAsPgOAOgiuxgPMYtq2x+/qmeKUf0jYS5lrK1K9lqr8QrRmIJBInEY9FYPJql7DXR+CYA3yr5Kmg0yO7zMZtoNwL0LBiAJQf+gH9BfW6BATxKeS9gTjYGSw4PDnuORZEPDwxNNUUGlw6aRwUtRQwvmiglBaOkLQoKCAUEBzsAq6wDr7KztCUEBLfBuwe9AL/BP7y1B78EqTTEy86MNHvUMyEAIfkECQAAEAAsCgAFABAAGgAABbMgJI4QYJpkag6D4wxAKgKs8Rh2rA746ZM0gwlH7M14ABxrydIFaS1HwgUb0QSCROKxaCweupIWm2iAEwCwtYwVNBrlN3qmTbgbgbo2DMiWA4CBgEB+b4IBMgB6dWFWSwUFLAWNJTUGAwUKOJNAPESYmgYKfJ4GLi4KqY1BDg+uDqkIqzytr6mcVjQMrbC3uAAEkAW7k8IEBDHAwZAoygQHyQfHyFbPB9Az0sdO1yi5J7kjIQAh+QQJAAAQACwKAAQADwAbAAAFsSAkjhBgmiRpDoPjDEBassZj1LE63Gc/AjvTbcibGQA3lpIVAx5ZroQLJgIKBInEY9FYPHKA7DXR+CYA3yr5Kmg0yO5zKZtoNwL0LBhLDvj/fj98boABKWF5eodKBQUsBTlVNAYDBQo3kD87Q5WXBgpgm5SdCqVgDC6UlgilCGAODw8upZYKmSWwsQ6lvJEAqLK8lrdVDAyNtMQ/jcyNkT8E0QQA0s8lB9jYANnPPd4oIQAh+QQJAAARACwJAAUAEAAaAAAFtWAkjiJgmmRqDoPjDEBassZj1LE63GdPAjvTbcibGQA3lpKVAx5ZroQLNgIKBInEY9FYPHIRQPaaaHwTgG+1fBU0GuU3upRNuBuBehYMwJYDgIGAP35vggEqYnp7OkoFBSyQKGE7QwMFCjcFDhArlkmYNwqdTgZLlwqpCgwmLi6PjwoIqQgxaQ8PDpiquwW2uLmxBAQACo+2DrkmwwQHAMdVKADNB86+MiXVy8TYJdvDYN09OSEAIfkECQAAEQAsCAAGABQAGQAABbpgJI4jYJpkSprD4DgDoKZAazzGLc9inZ9Ans+UK/5Uw1xr2dqtBoaay5F4xWY1gSCReCwai4dzxdUmGuIEQIw9awWNxjmuRnIT8Ebgzh2XtmcBgoOCSCaAcYQBECc9TAN8fSYOEA4sRi0FBZktlBCMaw5GBgMFCjkFOZSWaw8vmKY5CgAMKK2uRZkKCi2zJQ5iAJq7uyYKCH4oEcKaBQQEB8J+K83PB9HTNJrW0Tw0ANDd3kjX2ePKKiEAIfkEBQAAEQAsCQAFABAAGgAABbVgJI4iYJpkag6D4wxAWrLGY9SxOtxnTwI7023ImxkAN5aSlQMeWa6ECzYCCgSJxGPRWDxyEUD2mmh8E4BvtXwVNBrlN7qUTbgbgXoWDMCWA4CBgD9+b4IBKmJ6ezpKBQUskChhO0MDBQo3BQ4QK5ZJmDcKnU4GS5cKqQoMJi4uj48KCKkIMWkPDw6YqrsFtri5sQQEAAqPtg65JsMEBwDHVSgAzQfOvjIl1cvE2CXbw2DdPTkhADs="

                ( Right, _, _ ) ->
                    "R0lGODlhIwAjAPQAAAAAABIJBC4NAHYAAlEtEwMlYrwAA6tnOvj2L9iNZRBFoRSX25KOhv2yiMbHtfT15//4/wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh+QQJAAAQACwAAAAAIwAjAAAF1iAkjmRpnmiqrmzrvnBMAjQtq/QwOM4A3CeAzvAwEH/A2cBYayZFQgPNSGUmowCjbqtD3rA6XoLnewoFgkTisWgsHt6vGp1owBMAuLmOFjQadX94V2oJfg0BhWpxMgBpdQGRkpFPEI5qf5MBlVCKhYxAQjoFBaOgjUMGAwUKRgWnMFFUq60GCrAusqq0Cr24LQAMPKqsCL0IvywADg8PPL2sCq9mzM0OvdjJysLO2KzTlcEMpNDgnACk6aTauQTuBADv7MoH9fUA9vM4Tfz6nP8AAwo0EQIAOw=="
           )


gravity : Float -> Model -> Model
gravity adjustedLatency ({ velocity } as model) =
    { model
        | velocity =
            { velocity
                | vertical =
                    if model.position.top > 0 then
                        velocity.vertical - adjustedLatency / 4

                    else
                        0
            }
    }


jump : Model -> Model
jump ({ velocity, controls } as model) =
    if controls.up && velocity.vertical == 0 then
        { model | velocity = { velocity | vertical = 8 } }

    else
        model


walk : Model -> Model
walk ({ velocity, controls, face } as model) =
    { model
        | face = updateFace controls face
        , velocity = updateHorizontalVelocity controls velocity
    }


physics : Float -> Model -> Model
physics adjustedLatency ({ position, velocity, size } as model) =
    { model
        | position =
            { position
                | left = clamp 0 (toFloat size.width - 35) (position.left + adjustedLatency * velocity.horizontal)
                , top = clamp 0 (toFloat size.height - 35) (position.top + adjustedLatency * velocity.vertical)
            }
    }


updateFace : Controls -> HorizontalDirection -> HorizontalDirection
updateFace { left, right } face =
    if left then
        Left

    else if right then
        Right

    else
        face


updateHorizontalVelocity : Controls -> Velocity -> Velocity
updateHorizontalVelocity { left, right } velocity =
    { velocity
        | horizontal =
            if left then
                -1

            else if right then
                1

            else
                0
    }


updateControls : Bool -> Direction -> Controls -> Controls
updateControls isPressed direction controls =
    case direction of
        Vertical Up ->
            { controls | up = isPressed }

        Vertical Down ->
            { controls | down = isPressed }

        Horizontal Left ->
            { controls | left = isPressed }

        Horizontal Right ->
            { controls | right = isPressed }


toDirectionDecoder : String -> Decode.Decoder Direction
toDirectionDecoder text =
    case text of
        "ArrowUp" ->
            Decode.succeed (Vertical Up)

        "ArrowDown" ->
            Decode.succeed (Vertical Down)

        "ArrowLeft" ->
            Decode.succeed (Horizontal Left)

        "ArrowRight" ->
            Decode.succeed (Horizontal Right)

        _ ->
            Decode.fail ("not a direction: " ++ text)


directionToString : Direction -> String
directionToString dir =
    case dir of
        Vertical Up ->
            "ArrowUp"

        Vertical Down ->
            "ArrowDown"

        Horizontal Left ->
            "ArrowLeft"

        Horizontal Right ->
            "ArrowRight"


toPx : Float -> String
toPx n =
    String.fromFloat n ++ "px"


fromViewport : Browser.Dom.Viewport -> Msg
fromViewport { scene } =
    WindowResize (round scene.width) (round scene.height)


msgDecoder : Decode.Decoder Msg
msgDecoder =
    Decode.oneOf
        [ Decode.map NextFrame (Decode.field "Frame" Decode.float)
        , Decode.map2 WindowResize
            (Decode.at [ "Resize", "width" ] Decode.int)
            (Decode.at [ "Resize", "height" ] Decode.int)
        , Decode.map Press (Decode.field "Press" (Decode.andThen toDirectionDecoder Decode.string))
        , Decode.map Release (Decode.field "Release" (Decode.andThen toDirectionDecoder Decode.string))
        ]


encodeMsg : Msg -> Encode.Value
encodeMsg msg =
    case msg of
        NextFrame latency ->
            Encode.object [ ( "Frame", Encode.float latency ) ]

        WindowResize width height ->
            Encode.object
                [ ( "Resize"
                  , Encode.object
                        [ ( "width", Encode.int width )
                        , ( "height", Encode.int height )
                        ]
                  )
                ]

        Press direction ->
            Encode.object
                [ ( "Press"
                  , Encode.string (directionToString direction)
                  )
                ]

        Release direction ->
            Encode.object
                [ ( "Release"
                  , Encode.string (directionToString direction)
                  )
                ]


encodeModel : Model -> Encode.Value
encodeModel { face, position, velocity, size, controls } =
    Encode.object
        [ ( "face"
          , case face of
                Left ->
                    Encode.string "Left"

                Right ->
                    Encode.string "Right"
          )
        , ( "position"
          , Encode.object
                [ ( "left", Encode.float position.left )
                , ( "top", Encode.float position.top )
                ]
          )
        , ( "velocity"
          , Encode.object
                [ ( "vertical", Encode.float velocity.vertical )
                , ( "horizontal", Encode.float velocity.horizontal )
                ]
          )
        , ( "size"
          , Encode.object
                [ ( "width", Encode.int size.width )
                , ( "height", Encode.int size.height )
                ]
          )
        , ( "controls"
          , Encode.object
                [ ( "up", Encode.bool controls.up )
                , ( "down", Encode.bool controls.down )
                , ( "left", Encode.bool controls.left )
                , ( "right", Encode.bool controls.right )
                ]
          )
        ]
