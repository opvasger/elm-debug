module DevTools exposing (Config, Program, toDocument, toHtml, toInit, toMsg, toSubscriptions, toUpdate)

import Browser
import Browser.Dom
import Browser.Events
import DevTools.Elements as Elements
import Element
import File exposing (File)
import File.Download
import File.Select
import History exposing (History)
import Html exposing (Html)
import Json.Decode as Jd
import Json.Encode as Je
import Task exposing (Task)


type alias Program flags model msg =
    Platform.Program flags (Model model msg) (Msg model msg)


type alias Config flags model msg =
    { printModel : model -> String
    , encodeMsg : msg -> Je.Value
    , msgDecoder : Jd.Decoder msg
    , toSession : flags -> Maybe String
    , output : Je.Value -> Cmd (Msg model msg)
    }



-- Model


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
    , loadModelError : Maybe Jd.Error
    }


encodeModel : (msg -> Je.Value) -> Model model msg -> Je.Value
encodeModel encodeMsg model =
    Je.object
        [ ( "history", History.encode encodeMsg model.history )
        , ( "debuggerWidth", Je.int model.debuggerWidth )
        , ( "debuggerBodyHeight", Je.int model.debuggerBodyHeight )
        , ( "debuggerLeftPosition", Je.int model.debuggerLeftPosition )
        , ( "debuggerTopPosition", Je.int model.debuggerTopPosition )
        , ( "viewportHeight", Je.int model.viewportHeight )
        , ( "viewportWidth", Je.int model.viewportWidth )
        , ( "isModelOverlayed", Je.bool model.isModelOverlayed )
        ]


modelDecoder : History.ModelUpdater model msg -> Jd.Decoder msg -> model -> Jd.Decoder (Model model msg)
modelDecoder updateModel msgDecoder model =
    Jd.map8
        (\his dbw dbh dlp dtp vh vw imo ->
            { history = his
            , debuggerWidth = dbw
            , debuggerBodyHeight = dbh
            , debuggerLeftPosition = dlp
            , debuggerTopPosition = dtp
            , viewportHeight = vh
            , viewportWidth = vw
            , hoverTarget = Elements.noTarget
            , isModelOverlayed = imo
            , loadModelError = Nothing
            }
        )
        (Jd.field "history" (History.decoder updateModel msgDecoder model))
        (Jd.field "debuggerWidth" Jd.int)
        (Jd.field "debuggerBodyHeight" Jd.int)
        (Jd.field "debuggerLeftPosition" Jd.int)
        (Jd.field "debuggerTopPosition" Jd.int)
        (Jd.field "viewportHeight" Jd.int)
        (Jd.field "viewportWidth" Jd.int)
        (Jd.field "isModelOverlayed" Jd.bool)



-- Msg


type Msg model msg
    = AppMsg msg
    | InitAppMsg msg
    | ViewportResize Int Int
    | ReplayIndex Int
    | ToggleReplay
    | ToggleOverlay
    | HoverElement Elements.HoverTarget
    | SaveModel
    | SelectModel
    | LoadModel File
    | ModelLoaded (Result Jd.Error (Model model msg))
    | DoNothing


toMsg : msg -> Msg model msg
toMsg =
    AppMsg


viewportToMsg : Browser.Dom.Viewport -> Msg model msg
viewportToMsg { viewport } =
    ViewportResize (round viewport.width) (round viewport.height)



-- Init


toInit :
    { modelCmdPair : ( model, Cmd msg )
    , msgDecoder : Jd.Decoder msg
    , update : msg -> model -> ( model, Cmd msg )
    , session : Maybe String
    }
    -> ( Model model msg, Cmd (Msg model msg) )
toInit config =
    ( { history = History.init (Tuple.first config.modelCmdPair)
      , debuggerWidth = 200
      , debuggerBodyHeight = 300
      , debuggerLeftPosition = 300
      , debuggerTopPosition = 30
      , viewportHeight = 500
      , viewportWidth = 500
      , hoverTarget = Elements.noTarget
      , isModelOverlayed = False
      , loadModelError = Nothing
      }
    , Cmd.batch
        [ Cmd.map InitAppMsg (Tuple.second config.modelCmdPair)
        , Task.perform viewportToMsg Browser.Dom.getViewport
        ]
    )



-- Subs


toSubscriptions :
    { msgDecoder : Jd.Decoder msg
    , subscriptions : model -> Sub msg
    }
    -> Model model msg
    -> Sub (Msg model msg)
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
    { msgDecoder : Jd.Decoder msg
    , encodeMsg : msg -> Je.Value
    , update : msg -> model -> ( model, Cmd msg )
    , output : Je.Value -> Cmd (Msg model msg)
    }
    -> Msg model msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg model msg) )
toUpdate config msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

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

        HoverElement target ->
            ( { model | hoverTarget = target }, Cmd.none )

        SaveModel ->
            ( model
            , File.Download.string
                "devtools-session.json"
                "application/json"
                (Je.encode 0 (encodeModel config.encodeMsg model))
            )

        SelectModel ->
            ( model
            , File.Select.file [ "application/json" ] LoadModel
            )

        LoadModel file ->
            ( model
            , File.toString file
                |> Task.andThen (loadModelHelper config.update config.msgDecoder (History.initialModel model.history))
                |> Task.attempt ModelLoaded
            )

        ModelLoaded result ->
            case result of
                Ok loadedModel ->
                    ( loadedModel, Cmd.none )

                Err loadError ->
                    ( { model | loadModelError = Just loadError }, Cmd.none )


loadModelHelper :
    History.ModelUpdater model msg
    -> Jd.Decoder msg
    -> model
    -> String
    -> Task Jd.Error (Model model msg)
loadModelHelper modelUpdater msgDecoder initialModel string =
    case Jd.decodeString (modelDecoder modelUpdater msgDecoder initialModel) string of
        Ok model ->
            Task.succeed model

        Err error ->
            Task.fail error



-- View


toDocument :
    { encodeMsg : msg -> Je.Value
    , printModel : model -> String
    , view : model -> Browser.Document msg
    }
    -> Model model msg
    -> Browser.Document (Msg model msg)
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
    { encodeMsg : msg -> Je.Value
    , printModel : model -> String
    , view : model -> Html msg
    }
    -> Model model msg
    -> Html (Msg model msg)
toHtml config model =
    view config model (config.view (History.currentModel model.history))


view :
    { config
        | encodeMsg : msg -> Je.Value
        , printModel : model -> String
    }
    -> Model model msg
    -> Html msg
    -> Html (Msg model msg)
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
                    , hoverTargetMsg = HoverElement
                    , isModelOverlayed = model.isModelOverlayed
                    , toggleOverlayMsg = ToggleOverlay
                    , isReplaying = History.isReplaying model.history
                    , toggleReplayMsg = ToggleReplay
                    , currentModelIndex = History.currentIndex model.history
                    , modelIndexLength = History.length model.history
                    , changeModelIndexMsg = ReplayIndex
                    , selectModelMsg = SelectModel
                    , loadModelError = model.loadModelError
                    , saveModelMsg = SaveModel
                    }
                )
            )
        ]
        (Element.html (Html.map (toHtmlMsg model.history) html))


toHtmlMsg : History model msg -> (msg -> Msg model msg)
toHtmlMsg history =
    if History.isReplaying history then
        always DoNothing

    else
        AppMsg
