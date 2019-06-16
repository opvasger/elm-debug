module DevTools.Browser.Program exposing
    ( Model
    , Msg
    , Program
    , mapDocument
    , mapHtml
    , mapInit
    , mapSubscriptions
    , mapUpdate
    , mapUrlMsg
    )

import Browser
import File.Download
import History exposing (History)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode


type alias Program flags model msg =
    Platform.Program flags (Model model msg) (Msg model msg)


type Msg model msg
    = DoNothing
    | UpdateApp MsgSrc msg
    | ResetApp
    | ReplayApp Int
    | ToggleAppReplay
    | ToggleViewInteractive
    | DownloadSession


type alias Model model msg =
    { history : History model msg
    , initCmd : Cmd msg
    , isViewInteractive : Bool
    }


mapUrlMsg : msg -> Msg model msg
mapUrlMsg =
    UpdateApp Url


mapInit :
    { init : ( model, Cmd msg )
    , msgDecoder : Json.Decode.Decoder msg
    , update : msg -> model -> ( model, Cmd msg )
    , fromCache : Maybe Json.Encode.Value
    }
    -> ( Model model msg, Cmd (Msg model msg) )
mapInit config =
    ( { history = History.init (Tuple.first config.init)
      , initCmd = Tuple.second config.init
      , isViewInteractive = True
      }
    , Cmd.map (UpdateApp Init) (Tuple.second config.init)
    )


mapSubscriptions :
    { msgDecoder : Json.Decode.Decoder msg
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> Sub (Msg model msg)
mapSubscriptions config model =
    if History.isReplay model.history then
        Sub.none

    else
        Sub.map (UpdateApp Subs) (config.subscriptions (History.currentModel model.history))


mapUpdate :
    { msgDecoder : Json.Decode.Decoder msg
    , encodeMsg : msg -> Json.Encode.Value
    , update : msg -> model -> ( model, Cmd msg )
    , toCache : Json.Encode.Value -> Cmd (Msg model msg)
    }
    -> Msg model msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg model msg) )
mapUpdate config msg model =
    case msg of
        DoNothing ->
            ( model
            , Cmd.none
            )

        UpdateApp src appMsg ->
            ( { model | history = recordFromSrc src (withoutCmd config.update) appMsg model.history }
            , model.history
                |> History.currentModel
                |> config.update appMsg
                |> Tuple.second
                |> Cmd.map (UpdateApp Update)
            )

        ResetApp ->
            ( { model | history = History.reset model.history }
            , Cmd.map (UpdateApp Init) model.initCmd
            )

        ReplayApp index ->
            ( { model
                | history =
                    model.history
                        |> History.replay (withoutCmd config.update) index
              }
            , Cmd.none
            )

        ToggleViewInteractive ->
            ( { model | isViewInteractive = not model.isViewInteractive }
            , Cmd.none
            )

        ToggleAppReplay ->
            ( { model | history = History.toggleReplay (withoutCmd config.update) model.history }
            , Cmd.none
            )

        DownloadSession ->
            ( model
            , File.Download.string
                "devtools-session"
                "application/json"
                (Json.Encode.encode 0 (encodeSession config.encodeMsg model))
            )


mapDocument :
    { encodeMsg : msg -> Json.Encode.Value
    , printModel : model -> String
    , viewApp : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> Browser.Document (Msg model msg)
mapDocument config model =
    let
        { title, body } =
            config.viewApp (History.currentModel model.history)
    in
    { title = title
    , body =
        viewButton ResetApp "Reset"
            :: viewReplaySlider model.history
            :: viewButton ToggleViewInteractive
                (if model.isViewInteractive then
                    "Resume"

                 else
                    "Pause"
                )
            :: viewButton ToggleAppReplay
                (if History.isReplay model.history then
                    "Record"

                 else
                    "Replay"
                )
            :: viewButton DownloadSession "Download"
            :: viewHistoryState model.history
            :: List.map (Html.map (updateAppIf model.isViewInteractive)) body
    }


mapHtml :
    { encodeMsg : msg -> Json.Encode.Value
    , printModel : model -> String
    , viewApp : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> Html (Msg model msg)
mapHtml config model =
    Html.map (updateAppIf model.isViewInteractive) (config.viewApp (History.currentModel model.history))



-- MsgSrc


type MsgSrc
    = Init
    | Update
    | Subs
    | View
    | Url


recordFromSrc :
    MsgSrc
    -> (msg -> model -> model)
    -> msg
    -> History model msg
    -> History model msg
recordFromSrc src =
    case src of
        Init ->
            History.recordForever

        _ ->
            History.record



-- Helpers


encodeSession : (msg -> Json.Encode.Value) -> Model model msg -> Json.Encode.Value
encodeSession encodeMsg model =
    Json.Encode.object
        [ ( "history", History.encode encodeMsg model.history )
        , ( "isViewInteractive", Json.Encode.bool model.isViewInteractive )
        ]


updateAppIf : Bool -> msg -> Msg model msg
updateAppIf shouldUpdate =
    if shouldUpdate then
        UpdateApp View

    else
        always DoNothing


withoutCmd : (msg -> model -> ( model, Cmd msg )) -> msg -> model -> model
withoutCmd update msg model =
    Tuple.first (update msg model)


viewButton : Msg model msg -> String -> Html (Msg model msg)
viewButton msg text =
    Html.button
        [ Html.Events.onClick msg
        ]
        [ Html.text text
        ]


viewHistoryState : History model msg -> Html (Msg model msg)
viewHistoryState history =
    let
        currentIndex =
            History.currentIndex history

        length =
            History.length history

        children =
            if currentIndex == length then
                Html.text (String.fromInt length) :: []

            else
                [ Html.text (String.fromInt currentIndex)
                , Html.text "/"
                , Html.text (String.fromInt length)
                ]
    in
    Html.div [] children


viewPauseButton : Bool -> Html (Msg model msg)
viewPauseButton isViewInteractive =
    Html.button
        [ Html.Events.onClick ToggleViewInteractive
        ]
        [ if isViewInteractive then
            Html.text "Disable View Events"

          else
            Html.text "Enable View Events"
        ]


viewReplaySlider : History model msg -> Html (Msg model msg)
viewReplaySlider history =
    Html.input
        [ Html.Attributes.type_ "range"
        , Html.Attributes.step (String.fromInt 1)
        , Html.Attributes.min (String.fromInt 0)
        , Html.Attributes.max (String.fromInt (History.length history))
        , Html.Attributes.value (String.fromInt (History.currentIndex history))
        , Html.Events.onInput (Maybe.withDefault DoNothing << Maybe.map ReplayApp << String.toInt)
        ]
        []
