module Window exposing
    ( Model
    , Msg
    , decoder
    , encode
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Help exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Model =
    { isMoving : Bool
    , position : ( Int, Int )
    , size : ( Int, Int )
    }


type Msg
    = ToggleMoving
    | MoveTo ( Int, Int )


init : ( Model, Cmd Msg )
init =
    ( { isMoving = False
      , position = ( 0, 0 )
      , size = ( 200, 250 )
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isMoving then
        Sub.batch
            [ Browser.Events.onMouseUp
                (Decode.succeed ToggleMoving)
            , Sub.map MoveTo
                (Browser.Events.onMouseMove mousePositionDecoder)
            ]

    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleMoving ->
            ( { model | isMoving = not model.isMoving }
            , Cmd.none
            )

        MoveTo position ->
            ( { model | position = position }
            , Cmd.none
            )


view :
    (Msg -> msg)
    ->
        { top : List (Html msg)
        , mid : List (Html msg)
        , bot : List (Html msg)
        }
    -> Model
    -> Html msg
view msg children model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "position" "fixed"
        , style "background-color" backgroundColor
        , style "z-index" (String.fromInt zIndexMax)
        , style "border" borderStyle
        , style "left" (px (Tuple.first model.position))
        , style "top" (px (Tuple.second model.position))
        , style "width" (px (Tuple.first model.size))
        , style "height" (px (Tuple.second model.size))
        , on "mousedown" (Decode.succeed (msg ToggleMoving))
        ]
        [ div
            [ style "border-bottom" borderStyle
            , style "display" "flex"
            , style "flex-direction" "row"
            , style "padding" "2px"
            ]
            children.top
        , div
            [ style "background-color" "white"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "flex-grow" "1"
            ]
            children.mid
        , div
            [ style "border-top" borderStyle
            , style "display" "flex"
            , style "flex-direction" "row"
            , style "padding" "2px"
            ]
            children.bot
        ]



-- Json


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "position"
          , Encode.list Encode.int
                [ Tuple.first model.position
                , Tuple.second model.position
                ]
          )
        , ( "size"
          , Encode.list Encode.int
                [ Tuple.first model.size
                , Tuple.second model.size
                ]
          )
        ]


decoder : Decoder Model
decoder =
    Decode.map2
        (\position size ->
            { isMoving = False
            , position = position
            , size = size
            }
        )
        (Decode.field "position"
            (Decode.map2 Tuple.pair
                (Decode.index 0 Decode.int)
                (Decode.index 1 Decode.int)
            )
        )
        (Decode.field "size"
            (Decode.map2 Tuple.pair
                (Decode.index 0 Decode.int)
                (Decode.index 1 Decode.int)
            )
        )


mousePositionDecoder : Decoder ( Int, Int )
mousePositionDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)
