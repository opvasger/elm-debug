module DevTools.Browser.Window exposing
    ( Model
    , Msg
    , decoder
    , encode
    , ignoreSelectOnMove
    , init
    , isMoving
    , subscriptions
    , update
    , view
    )

import Browser.Dom
import Browser.Events
import Html exposing (Html, div, span)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task


type alias Model =
    { viewport : ( Int, Int )
    , position : ( Int, Int )
    , size : ( Int, Int )
    , movePosition : Maybe ( Int, Int )
    , isCollapsed : Bool
    }


type Msg
    = MoveTo ( Int, Int )
    | StopMove
    | ResizeViewport ( Int, Int )
    | ToggleCollapsed
    | Dismiss


collapsedHeight : Int
collapsedHeight =
    25


isMoving : Model -> Bool
isMoving { movePosition } =
    movePosition /= Nothing


ignoreSelectOnMove : Model -> Html msg -> Html msg
ignoreSelectOnMove window html =
    if isMoving window then
        span
            [ style "-webkit-touch-callout" "none"
            , style "-webkit-user-select" "none"
            , style "-khtml-user-select" "none"
            , style "-moz-user-select" "none"
            , style "-ms-user-select" "none"
            , style "user-select" "none"
            ]
            [ html
            ]

    else
        html


init : Bool -> ( Model, Cmd Msg )
init isCollapsed =
    ( { position = ( 50, 50 )
      , movePosition = Nothing
      , size = ( 200, 250 )
      , viewport = ( 500, 500 )
      , isCollapsed = isCollapsed
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
                            model.position
                                |> clampPos ( 0, 0 )
                                    (diffPos
                                        (shrinkCollapsed model.isCollapsed
                                            model.size
                                        )
                                        model.viewport
                                    )
                                |> diffPos (diffPos position movePosition)

                        Nothing ->
                            model.position
            }

        StopMove ->
            { model | movePosition = Nothing }

        ResizeViewport viewport ->
            { model | viewport = viewport }

        ToggleCollapsed ->
            { model | isCollapsed = not model.isCollapsed }

        Dismiss ->
            { model
                | isCollapsed = True
                , position = model.viewport
            }


shrinkCollapsed : Bool -> ( Int, Int ) -> ( Int, Int )
shrinkCollapsed isCollapsed ( width, height ) =
    if isCollapsed then
        ( width, collapsedHeight )

    else
        ( width, height )


view :
    (Msg -> msg)
    -> Model
    ->
        { collapsed : Msg -> List (Html msg)
        , expanded :
            { head : Msg -> Msg -> List (Html msg)
            , body : List (Html msg)
            , foot : List (Html msg)
            }
        }
    -> Html msg
view msg model { collapsed, expanded } =
    let
        ( width, height ) =
            if model.isCollapsed then
                ( Tuple.first model.size, collapsedHeight )

            else
                model.size

        ( left, top ) =
            clampPos ( 0, 0 )
                (model.viewport
                    |> diffPos (shrinkCollapsed model.isCollapsed model.size)
                    |> diffPos ( 20, 20 )
                )
                model.position
    in
    if model.isCollapsed then
        div
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "justify-content" "flex-end"
            , style "align-items" "center"
            , style "position" "fixed"
            , style "border" "1px solid #cccccc"
            , style "background-color" "#f3f3f3"
            , style "transition" "box-shadow .25s .25s ease-out, transform .25s .25s ease-out"
            , style "box-shadow"
                (if isMoving model then
                    "0px 1px 8px -4px"

                 else
                    "0px 0px 6px -4px"
                )
            , style "transform"
                (if isMoving model then
                    "translate(0px, -1px)"

                 else
                    "translate(0px, 0px)"
                )
            , style "z-index" (String.fromInt 2147483647)
            , style "left" (px left)
            , style "top" (px top)
            , style "width" (px width)
            , style "height" (px height)
            , on "mousedown" (Decode.map (msg << MoveTo) mouseEventPositionDecoder)
            , style "cursor"
                (model.movePosition
                    |> Maybe.map (always "grabbing")
                    |> Maybe.withDefault "grab"
                )
            ]
            (collapsed ToggleCollapsed)

    else
        div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "position" "fixed"
            , style "border" "1px solid #cccccc"
            , style "background-color" "#f3f3f3"
            , style "z-index" (String.fromInt 2147483647)
            , style "left" (px left)
            , style "top" (px top)
            , style "width" (px width)
            , style "height" (px height)
            , style "transition" "box-shadow .25s ease-out, transform .25s ease-out"
            , style "box-shadow"
                (if isMoving model then
                    "0px 1px 8px -4px"

                 else
                    "0px 0px 6px -4px"
                )
            , style "transform"
                (if isMoving model then
                    "translate(0px, -1px)"

                 else
                    "translate(0px, 0px)"
                )
            ]
            [ div
                [ style "display" "flex"
                , style "flex-direction" "row"
                , style "justify-content" "flex-end"
                , style "align-items" "center"
                , style "height" (px 25)
                , style "border-bottom" "1px solid #cccccc"
                , on "mousedown" (Decode.map (msg << MoveTo) mouseEventPositionDecoder)
                , style "cursor"
                    (model.movePosition
                        |> Maybe.map (always "grabbing")
                        |> Maybe.withDefault "grab"
                    )
                ]
                (expanded.head ToggleCollapsed Dismiss)
            , div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "flex-grow" "1"
                , style "background-color" "white"
                ]
                expanded.body
            , div
                [ style "display" "flex"
                , style "flex-direction" "row"
                , style "justify-content" "flex-end"
                , style "align-items" "center"
                , style "height" (px 25)
                , style "border-top" "1px solid #cccccc"
                ]
                expanded.foot
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
        , ( "isCollapsed"
          , Encode.bool model.isCollapsed
          )
        ]


decoder : Decoder Model
decoder =
    Decode.map4
        (\position viewport size isCollapsed ->
            { position = position
            , movePosition = Nothing
            , size = size
            , viewport = viewport
            , isCollapsed = isCollapsed
            }
        )
        (Decode.field "position" intPairDecoder)
        (Decode.field "viewport" intPairDecoder)
        (Decode.field "size" intPairDecoder)
        (Decode.field "isCollapsed" Decode.bool)


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


diffPos : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
diffPos ( a, b ) ( x, y ) =
    ( x - a, y - b )


clampPos : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
clampPos ( minA, minB ) ( maxA, maxB ) ( x, y ) =
    ( clamp minA maxA x
    , clamp minB maxB y
    )



--


px : Int -> String
px n =
    String.fromInt n ++ "px"
