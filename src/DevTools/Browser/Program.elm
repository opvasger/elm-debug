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
import File exposing (File)
import File.Download
import File.Select
import History exposing (History)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)
import Throttle exposing (Throttle)


type alias Program flags model msg =
    Platform.Program flags (Model model msg) (Msg model msg)


type Msg model msg
    = DoNothing
    | UpdateApp MsgSrc msg
    | ResetApp
    | ReplayApp Int
    | ToggleAppReplay
    | ToggleViewInteractive
    | ToggleDecodeStrategy
    | ToggleModelVisibility
    | DownloadSession
    | SelectSession
    | DecodeSession File
    | SessionDecoded (Result Decode.Error (Model model msg))
    | InputDescription String
    | UpdateCacheThrottle Throttle.Tick


type alias Model model msg =
    { history : History model msg
    , initCmd : Cmd msg
    , isViewInteractive : Bool
    , isModelVisible : Bool
    , decodeStrategy : DecodeStrategy
    , decodeError : Maybe Decode.Error
    , description : String
    , cacheThrottle : Throttle
    }


mapUrlMsg : msg -> Msg model msg
mapUrlMsg =
    UpdateApp Url


mapInit :
    { init : ( model, Cmd msg )
    , msgDecoder : Decoder msg
    , update : msg -> model -> ( model, Cmd msg )
    , fromCache : Maybe Encode.Value
    }
    -> ( Model model msg, Cmd (Msg model msg) )
mapInit config =
    ( { history = History.init (Tuple.first config.init)
      , initCmd = Tuple.second config.init
      , isViewInteractive = True
      , decodeError = Nothing
      , decodeStrategy = UntilError
      , description = ""
      , isModelVisible = False
      , cacheThrottle = Throttle.init
      }
    , Cmd.map (UpdateApp Init) (Tuple.second config.init)
    )


mapSubscriptions :
    { msgDecoder : Decoder msg
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
    { msgDecoder : Decoder msg
    , encodeMsg : msg -> Encode.Value
    , update : msg -> model -> ( model, Cmd msg )
    , toCache : Encode.Value -> Cmd (Msg model msg)
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
            tryCacheSession config.toCache config.encodeMsg <|
                ( { model
                    | history = recordFromSrc src (withoutCmd config.update) appMsg model.history
                  }
                , model.history
                    |> History.currentModel
                    |> config.update appMsg
                    |> Tuple.second
                    |> Cmd.map (UpdateApp Update)
                )

        ResetApp ->
            tryCacheSession config.toCache config.encodeMsg <|
                ( { model | history = History.reset model.history }
                , Cmd.map (UpdateApp Init) model.initCmd
                )

        ReplayApp index ->
            tryCacheSession config.toCache config.encodeMsg <|
                ( { model
                    | history =
                        model.history
                            |> History.replay (withoutCmd config.update) index
                  }
                , Cmd.none
                )

        ToggleViewInteractive ->
            tryCacheSession config.toCache config.encodeMsg <|
                ( { model | isViewInteractive = not model.isViewInteractive }
                , Cmd.none
                )

        ToggleAppReplay ->
            tryCacheSession config.toCache config.encodeMsg <|
                ( { model | history = History.toggleReplay (withoutCmd config.update) model.history }
                , Cmd.none
                )

        DownloadSession ->
            ( model
            , File.Download.string
                "devtools-session"
                "application/json"
                (Encode.encode 0 (encodeSession config.encodeMsg model))
            )

        SelectSession ->
            ( model
            , File.Select.file [ "application/json" ] DecodeSession
            )

        DecodeSession file ->
            ( model
            , File.toString file
                |> Task.map
                    (model
                        |> sessionDecoder (withoutCmd config.update) config.msgDecoder NoErrors
                        |> Decode.decodeString
                    )
                |> Task.andThen resultToTask
                |> Task.attempt SessionDecoded
            )

        SessionDecoded result ->
            case result of
                Ok sessionModel ->
                    tryCacheSession config.toCache config.encodeMsg <|
                        ( sessionModel
                        , Cmd.map (UpdateApp Init) model.initCmd
                        )

                Err error ->
                    ( { model | decodeError = Just error }, Cmd.none )

        ToggleDecodeStrategy ->
            tryCacheSession config.toCache config.encodeMsg <|
                ( { model | decodeStrategy = nextDecodeStrategy model.decodeStrategy }, Cmd.none )

        ToggleModelVisibility ->
            tryCacheSession config.toCache config.encodeMsg <|
                ( { model | isModelVisible = not model.isModelVisible }, Cmd.none )

        InputDescription text ->
            tryCacheSession config.toCache config.encodeMsg <|
                ( { model | description = text }, Cmd.none )

        UpdateCacheThrottle tick ->
            Tuple.mapFirst
                (\cacheThrottle -> { model | cacheThrottle = cacheThrottle })
                (Throttle.update UpdateCacheThrottle
                    (config.toCache (encodeSession config.encodeMsg model))
                    tick
                    model.cacheThrottle
                )


mapDocument :
    { encodeMsg : msg -> Encode.Value
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
        viewReplaySlider model.history
            :: viewButton ResetApp "Reset"
            :: viewButton ToggleAppReplay
                (if History.isReplay model.history then
                    "Paused"

                 else
                    "Recoding"
                )
            :: viewButton DownloadSession "Download"
            :: viewButton SelectSession "Upload"
            :: viewButton ToggleDecodeStrategy
                (case model.decodeStrategy of
                    NoErrors ->
                        "Upload all message with no errors"

                    UntilError ->
                        "Upload messages until first error"

                    SkipErrors ->
                        "Upload messages and skip errors"
                )
            :: viewButton ToggleViewInteractive
                (if model.isViewInteractive then
                    "View Events Enabled"

                 else
                    "View Events Disabled"
                )
            :: viewButton ToggleModelVisibility
                (if model.isModelVisible then
                    "Showing Model Overlay"

                 else
                    "Hiding Model Overlay"
                )
            :: viewStateCount model.history
            :: viewDescription model.description
            :: viewDecodeError model.decodeError
            :: viewModel config.printModel model.history model.isModelVisible
            :: List.map (Html.map (updateAppIf model.isViewInteractive)) body
    }


mapHtml :
    { encodeMsg : msg -> Encode.Value
    , printModel : model -> String
    , viewApp : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> Html (Msg model msg)
mapHtml config model =
    Html.map (updateAppIf model.isViewInteractive) (config.viewApp (History.currentModel model.history))



-- DecodeStrategy


type DecodeStrategy
    = NoErrors
    | UntilError
    | SkipErrors


encodeDecodeStrategy : DecodeStrategy -> Encode.Value
encodeDecodeStrategy strategy =
    case strategy of
        NoErrors ->
            Encode.string "UntilError"

        UntilError ->
            Encode.string "SkipErrors"

        SkipErrors ->
            Encode.string "NoErrors"


decodeStrategyDecoder : Decoder DecodeStrategy
decodeStrategyDecoder =
    Decode.andThen
        (\text ->
            case text of
                "NoErrors" ->
                    Decode.succeed NoErrors

                "UntilError" ->
                    Decode.succeed UntilError

                "SkipErrors" ->
                    Decode.succeed SkipErrors

                _ ->
                    Decode.fail (text ++ " should be either 'NoErrors', 'UntilError', or 'SkipErrors'")
        )
        Decode.string


nextDecodeStrategy : DecodeStrategy -> DecodeStrategy
nextDecodeStrategy strategy =
    case strategy of
        NoErrors ->
            UntilError

        UntilError ->
            SkipErrors

        SkipErrors ->
            NoErrors


toHistoryDecoder :
    DecodeStrategy
    -> (msg -> model -> model)
    -> Decoder msg
    -> History model msg
    -> Decoder (History model msg)
toHistoryDecoder strategy =
    case strategy of
        NoErrors ->
            History.noErrorsDecoder

        UntilError ->
            History.untilErrorDecoder

        SkipErrors ->
            History.skipErrorsDecoder



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


tryCacheSession :
    (Encode.Value -> Cmd (Msg model msg))
    -> (msg -> Encode.Value)
    -> ( Model model msg, Cmd (Msg model msg) )
    -> ( Model model msg, Cmd (Msg model msg) )
tryCacheSession toCache encodeMsg ( model, cmd ) =
    let
        ( cacheThrottle, cacheSession ) =
            Throttle.try UpdateCacheThrottle
                (toCache (encodeSession encodeMsg model))
                model.cacheThrottle
    in
    ( { model | cacheThrottle = cacheThrottle }
    , Cmd.batch [ cmd, cacheSession ]
    )


encodeSession : (msg -> Encode.Value) -> Model model msg -> Encode.Value
encodeSession encodeMsg model =
    Encode.object
        [ ( "history", History.encode encodeMsg model.history )
        , ( "isViewInteractive", Encode.bool model.isViewInteractive )
        , ( "isModelVisible", Encode.bool model.isModelVisible )
        , ( "decodeStrategy", encodeDecodeStrategy model.decodeStrategy )
        , ( "description", Encode.string model.description )
        ]


sessionDecoder :
    (msg -> model -> model)
    -> Decoder msg
    -> DecodeStrategy
    -> Model model msg
    -> Decoder (Model model msg)
sessionDecoder update msgDecoder strategy model =
    Decode.map5
        (\history isViewInteractive decodeStrategy description isModelVisible ->
            { history = history
            , initCmd = model.initCmd
            , isViewInteractive = isViewInteractive
            , decodeError = Nothing
            , decodeStrategy = decodeStrategy
            , description = description
            , isModelVisible = isModelVisible
            , cacheThrottle = Throttle.init
            }
        )
        (Decode.field "history" (toHistoryDecoder strategy update msgDecoder model.history))
        (Decode.field "isViewInteractive" Decode.bool)
        (Decode.field "decodeStrategy" decodeStrategyDecoder)
        (Decode.field "description" Decode.string)
        (Decode.field "isModelVisible" Decode.bool)


resultToTask : Result err ok -> Task err ok
resultToTask result =
    case result of
        Ok value ->
            Task.succeed value

        Err error ->
            Task.fail error


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


viewModel : (model -> String) -> History model msg -> Bool -> Html (Msg model msg)
viewModel printModel history isModelVisible =
    if isModelVisible then
        Html.div []
            [ Html.text (printModel (History.currentModel history))
            ]

    else
        Html.text ""


viewDescription : String -> Html (Msg model msg)
viewDescription text =
    Html.textarea
        [ Html.Attributes.value text
        , Html.Events.onInput InputDescription
        , Html.Attributes.placeholder "You can describe what you're doing here!"
        ]
        []


viewDecodeError : Maybe Decode.Error -> Html msg
viewDecodeError maybe =
    case maybe of
        Just error ->
            Html.div [] [ Html.text (Decode.errorToString error) ]

        Nothing ->
            Html.text ""


viewStateCount : History model msg -> Html (Msg model msg)
viewStateCount history =
    let
        currentIndex =
            History.currentIndex history

        length =
            History.length history

        children =
            if currentIndex == length then
                Html.text (String.fromInt (length + 1)) :: []

            else
                [ Html.text (String.fromInt (currentIndex + 1))
                , Html.text "/"
                , Html.text (String.fromInt (length + 1))
                ]
    in
    Html.div [] children


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
