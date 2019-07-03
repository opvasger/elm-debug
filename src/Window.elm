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

import Browser.Dom
import Browser.Events
import Help exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task


type alias Model =
    { position : ( Int, Int )
    , movePosition : Maybe ( Int, Int )
    , size : ( Int, Int )
    , viewport : ( Int, Int )
    }


type Msg
    = MoveTo ( Int, Int )
    | StopMove
    | ResizeViewport ( Int, Int )


init : ( Model, Cmd Msg )
init =
    ( { position = ( 50, 50 )
      , movePosition = Nothing
      , size = ( 200, 250 )
      , viewport = ( 500, 500 )
      }
    , Task.perform (ResizeViewport << viewportToSize)
        Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ResizeViewport
            (Browser.Events.onResize Tuple.pair)
        , if model.movePosition /= Nothing then
            Sub.batch
                [ Browser.Events.onMouseUp (Decode.succeed StopMove)
                , Sub.map MoveTo
                    (Browser.Events.onMouseMove mouseEventPositionDecoder)
                ]

          else
            Sub.none
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        MoveTo position ->
            { model
                | movePosition = Just position
                , position =
                    case model.movePosition of
                        Just movePosition ->
                            diffPos (diffPos position movePosition)
                                model.position

                        Nothing ->
                            model.position
            }

        StopMove ->
            { model | movePosition = Nothing }

        ResizeViewport viewport ->
            { model | viewport = viewport }


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
    let
        clampedPosition =
            clampPos ( 0, 0 ) (diffPos model.size model.viewport) model.position
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "position" "fixed"
        , style "border" borderStyle
        , style "background-color" backgroundColor
        , style "z-index" (String.fromInt zIndexMax)
        , style "left" (px (Tuple.first clampedPosition))
        , style "top" (px (Tuple.second clampedPosition))
        , style "width" (px (Tuple.first model.size))
        , style "height" (px (Tuple.second model.size))
        ]
        [ div
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "padding" "2px"
            , style "border-bottom" borderStyle
            , style "cursor"
                (if model.movePosition /= Nothing then
                    "grabbing"

                 else
                    "grab"
                )
            , on "mousedown" (Decode.map (msg << MoveTo) mouseEventPositionDecoder)
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
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "padding" "2px"
            , style "border-top" borderStyle
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
        , ( "viewport"
          , Encode.list Encode.int
                [ Tuple.first model.viewport
                , Tuple.second model.viewport
                ]
          )
        ]


decoder : Decoder Model
decoder =
    Decode.map3
        (\position size viewport ->
            { position = position
            , movePosition = Nothing
            , size = size
            , viewport = viewport
            }
        )
        (Decode.field "position" intPairDecoder)
        (Decode.field "size" intPairDecoder)
        (Decode.field "viewport" intPairDecoder)


mouseEventPositionDecoder : Decoder ( Int, Int )
mouseEventPositionDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)


intPairDecoder : Decoder ( Int, Int )
intPairDecoder =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)


viewportToSize : Browser.Dom.Viewport -> ( Int, Int )
viewportToSize { viewport } =
    ( round viewport.width, round viewport.height )



-- Position


type alias Position =
    ( Int, Int )


diffPos : Position -> Position -> Position
diffPos ( a, b ) ( x, y ) =
    ( x - a, y - b )


clampPos : Position -> Position -> Position -> Position
clampPos ( minA, minB ) ( maxA, maxB ) ( x, y ) =
    ( clamp minA maxA x
    , clamp minB maxB y
    )
