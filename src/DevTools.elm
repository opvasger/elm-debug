module DevTools exposing (Config, Program, toDocument, toHtml, toInit, toMsg, toSubscriptions, toUpdate)

import Browser
import Browser.Dom
import Browser.Events
import DevTools.Elements as Elements
import Element
import History exposing (History)
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Task


type alias Program flags model msg =
    Platform.Program flags (Model model msg) (Msg msg)


type alias Config flags model msg =
    { printModel : model -> String
    , encodeMsg : msg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder msg
    , toSession : flags -> Maybe String
    , output : Json.Encode.Value -> Cmd (Msg msg)
    }


type alias Model model msg =
    { history : History model msg
    , debuggerWidth : Int
    , debuggerBodyHeight : Int
    , debuggerLeftPosition : Int
    , debuggerTopPosition : Int
    , viewportHeight : Int
    , viewportWidth : Int
    , hoverTarget : Elements.HoverTarget
    , isModelOverlayed : Bool
    }



-- Msg


type Msg msg
    = AppMsg msg
    | InitAppMsg msg
    | ViewportResize Int Int
    | ReplayIndex Int
    | ToggleReplay
    | ToggleOverlay
    | Hover Elements.HoverTarget
    | DoNothing


toMsg : msg -> Msg msg
toMsg =
    AppMsg


viewportToMsg : Browser.Dom.Viewport -> Msg msg
viewportToMsg { viewport } =
    ViewportResize (round viewport.width) (round viewport.height)



-- Init


toInit :
    { modelCmdPair : ( model, Cmd msg )
    , msgDecoder : Json.Decode.Decoder msg
    , update : msg -> model -> ( model, Cmd msg )
    , session : Maybe String
    }
    -> ( Model model msg, Cmd (Msg msg) )
toInit config =
    ( { history = History.init (Tuple.first config.modelCmdPair)
      , debuggerWidth = 200
      , debuggerBodyHeight = 300
      , debuggerLeftPosition = 500
      , debuggerTopPosition = 500
      , viewportHeight = 500
      , viewportWidth = 500
      , hoverTarget = Elements.noTarget
      , isModelOverlayed = False
      }
    , Cmd.batch
        [ Cmd.map InitAppMsg (Tuple.second config.modelCmdPair)
        , Task.perform viewportToMsg Browser.Dom.getViewport
        ]
    )



-- Subs


toSubscriptions :
    { msgDecoder : Json.Decode.Decoder msg
    , subscriptions : model -> Sub msg
    }
    -> Model model msg
    -> Sub (Msg msg)
toSubscriptions config model =
    Sub.batch
        [ Browser.Events.onResize ViewportResize
        , if History.isReplaying model.history then
            Sub.none

          else
            Sub.map AppMsg (config.subscriptions (History.currentModel model.history))
        ]



-- Update


toUpdate :
    { msgDecoder : Json.Decode.Decoder msg
    , encodeMsg : msg -> Json.Encode.Value
    , update : msg -> model -> ( model, Cmd msg )
    , output : Json.Encode.Value -> Cmd (Msg msg)
    }
    -> Msg msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg msg) )
toUpdate config msg model =
    case msg of
        AppMsg appMsg ->
            let
                ( history, cmd ) =
                    History.update config.update appMsg model.history
            in
            ( { model | history = history }, Cmd.map AppMsg cmd )

        InitAppMsg appMsg ->
            let
                ( history, cmd ) =
                    History.updateAndPersist config.update appMsg model.history
            in
            ( { model | history = history }, Cmd.map AppMsg cmd )

        ViewportResize width height ->
            ( { model
                | viewportWidth = width
                , viewportHeight = height
              }
            , Cmd.none
            )

        ReplayIndex index ->
            ( { model | history = History.replay config.update index model.history }
            , Cmd.none
            )

        ToggleReplay ->
            ( { model | history = History.toggleState config.update model.history }
            , Cmd.none
            )

        ToggleOverlay ->
            ( { model | isModelOverlayed = not model.isModelOverlayed }, Cmd.none )

        Hover target ->
            ( { model | hoverTarget = target }, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )



-- View


toDocument :
    { encodeMsg : msg -> Json.Encode.Value
    , printModel : model -> String
    , view : model -> Browser.Document msg
    }
    -> Model model msg
    -> Browser.Document (Msg msg)
toDocument config model =
    let
        { title, body } =
            config.view (History.currentModel model.history)
    in
    { title = title
    , body =
        [ view config model (Html.div [] body)
        ]
    }


toHtml :
    { encodeMsg : msg -> Json.Encode.Value
    , printModel : model -> String
    , view : model -> Html msg
    }
    -> Model model msg
    -> Html (Msg msg)
toHtml config model =
    view config model (config.view (History.currentModel model.history))


view :
    { config
        | encodeMsg : msg -> Json.Encode.Value
        , printModel : model -> String
    }
    -> Model model msg
    -> Html msg
    -> Html (Msg msg)
view config model html =
    Element.layout
        [ Element.inFront
            (Element.el
                [ Element.behindContent
                    (Elements.viewModelOverlay
                        { isEnabled = model.isModelOverlayed
                        , printModel = config.printModel
                        , model = History.currentModel model.history
                        }
                    )
                ]
                (Elements.viewDebugger
                    { width = model.debuggerWidth
                    , bodyHeight = model.debuggerBodyHeight
                    , leftPosition = model.debuggerLeftPosition
                    , topPosition = model.debuggerTopPosition
                    , hoverTarget = model.hoverTarget
                    , hoverTargetMsg = Hover
                    , isModelOverlayed = model.isModelOverlayed
                    , toggleOverlayMsg = ToggleOverlay
                    , isReplaying = History.isReplaying model.history
                    , toggleReplayMsg = ToggleReplay
                    , currentModelIndex = History.currentIndex model.history
                    , modelIndexLength = History.length model.history
                    , changeModelIndexMsg = ReplayIndex
                    }
                )
            )
        ]
        (Element.html (Html.map (toHtmlMsg model.history) html))


toHtmlMsg : History model msg -> (msg -> Msg msg)
toHtmlMsg history =
    if History.isReplaying history then
        always DoNothing

    else
        AppMsg
