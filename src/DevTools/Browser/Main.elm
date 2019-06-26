module DevTools.Browser.Main exposing
    ( Model
    , Msg
    , Program
    , toDocument
    , toHtml
    , toInit
    , toSubscriptions
    , toUpdate
    , toUrlMsg
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
import Throttle


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
    | ToggleCacheSession
    | DownloadSession
    | SelectSession
    | DecodeSession File
    | SessionDecoded (Result Decode.Error (Model model msg))
    | InputDescription String
    | UpdateCacheThrottle


type alias Model model msg =
    { history : History model msg
    , initCmd : Cmd msg
    , isViewInteractive : Bool
    , isModelVisible : Bool
    , isCachingSession : Bool
    , decodeStrategy : DecodeStrategy
    , decodeError : Maybe ( SessionSrc, Decode.Error )
    , description : String
    , cacheThrottle : Throttle.Model
    }


toUrlMsg : msg -> Msg model msg
toUrlMsg =
    UpdateApp Url


toInit :
    { init : ( model, Cmd msg )
    , msgDecoder : Decoder msg
    , update : msg -> model -> ( model, Cmd msg )
    , fromCache : Maybe String
    }
    -> ( Model model msg, Cmd (Msg model msg) )
toInit config =
    let
        decodeStrategy =
            config.fromCache
                |> Maybe.map (Decode.decodeString (Decode.field "decodeStrategy" decodeStrategyDecoder))
                |> Maybe.map (Result.withDefault NoErrors)
                |> Maybe.withDefault NoErrors

        decodeSession =
            config.init
                |> sessionDecoder (ignoreCmd config.update) config.msgDecoder decodeStrategy

        toModel decodeError =
            { history = History.init (Tuple.first config.init)
            , initCmd = Tuple.second config.init
            , isViewInteractive = True
            , decodeError = Maybe.map (Tuple.pair Cache) decodeError
            , decodeStrategy = UntilError
            , description = ""
            , isModelVisible = False
            , cacheThrottle = Throttle.init
            , isCachingSession = True
            }
    in
    config.fromCache
        |> Maybe.map (noCmd << unwrapResult (toModel << Just) << Decode.decodeString decodeSession)
        |> Maybe.withDefault
            ( toModel Nothing
            , Cmd.map (UpdateApp Init) (Tuple.second config.init)
            )


toSubscriptions :
    { msgDecoder : Decoder msg
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> Sub (Msg model msg)
toSubscriptions config model =
    if History.isReplay model.history then
        Sub.none

    else
        Sub.map (UpdateApp Subs) (config.subscriptions (History.currentModel model.history))


toUpdate :
    { msgDecoder : Decoder msg
    , encodeMsg : msg -> Encode.Value
    , update : msg -> model -> ( model, Cmd msg )
    , toCache : String -> Cmd (Msg model msg)
    }
    -> Msg model msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg model msg) )
toUpdate config msg model =
    case msg of
        DoNothing ->
            noCmd model

        UpdateApp src appMsg ->
            History.currentModel model.history
                |> config.update appMsg
                |> Tuple.second
                |> Cmd.map (UpdateApp Update)
                |> Tuple.pair { model | history = recordFromSrc src (ignoreCmd config.update) appMsg model.history }
                |> emitCacheSession config.toCache config.encodeMsg model.isCachingSession

        ResetApp ->
            Cmd.map (UpdateApp Init) model.initCmd
                |> Tuple.pair
                    { model
                        | history = History.reset model.history
                        , decodeError = Nothing
                    }
                |> emitCacheSession config.toCache config.encodeMsg model.isCachingSession

        ReplayApp index ->
            { model | history = History.replay (ignoreCmd config.update) index model.history }
                |> noCmd
                |> emitCacheSession config.toCache config.encodeMsg model.isCachingSession

        ToggleViewInteractive ->
            { model | isViewInteractive = not model.isViewInteractive }
                |> noCmd
                |> emitCacheSession config.toCache config.encodeMsg model.isCachingSession

        ToggleAppReplay ->
            { model | history = History.toggleReplay (ignoreCmd config.update) model.history }
                |> noCmd
                |> emitCacheSession config.toCache config.encodeMsg model.isCachingSession

        DownloadSession ->
            encodeSession config.encodeMsg model
                |> File.Download.string "devtools-session" "application/json"
                |> Tuple.pair model

        SelectSession ->
            DecodeSession
                |> File.Select.file [ "application/json" ]
                |> Tuple.pair model

        DecodeSession file ->
            let
                decodeSession =
                    model.initCmd
                        |> Tuple.pair (History.initialModel model.history)
                        |> sessionDecoder (ignoreCmd config.update) config.msgDecoder NoErrors
            in
            File.toString file
                |> Task.map (Decode.decodeString decodeSession)
                |> Task.andThen resultToTask
                |> Task.attempt SessionDecoded
                |> Tuple.pair model

        SessionDecoded result ->
            case result of
                Ok sessionModel ->
                    noCmd sessionModel
                        |> emitCacheSession config.toCache config.encodeMsg model.isCachingSession

                Err error ->
                    noCmd { model | decodeError = Just ( Upload, error ) }

        ToggleDecodeStrategy ->
            { model | decodeStrategy = nextDecodeStrategy model.decodeStrategy }
                |> noCmd
                |> emitCacheSession config.toCache config.encodeMsg model.isCachingSession

        ToggleModelVisibility ->
            { model | isModelVisible = not model.isModelVisible }
                |> noCmd
                |> emitCacheSession config.toCache config.encodeMsg model.isCachingSession

        ToggleCacheSession ->
            { model | isCachingSession = not model.isCachingSession }
                |> noCmd
                |> emitCacheSession config.toCache config.encodeMsg model.isCachingSession

        InputDescription text ->
            { model | description = text }
                |> noCmd
                |> emitCacheSession config.toCache config.encodeMsg model.isCachingSession

        UpdateCacheThrottle ->
            Throttle.update (config.toCache << encodeSession config.encodeMsg)
                UpdateCacheThrottle
                model.cacheThrottle
                model
                |> Tuple.mapFirst (\cacheThrottle -> { model | cacheThrottle = cacheThrottle })


toDocument :
    { encodeMsg : msg -> Encode.Value
    , printModel : model -> String
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> Browser.Document (Msg model msg)
toDocument config model =
    History.currentModel model.history
        |> config.view
        |> (\{ title, body } ->
                { title = title
                , body = view config model body
                }
           )


toHtml :
    { encodeMsg : msg -> Encode.Value
    , printModel : model -> String
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> Html (Msg model msg)
toHtml config model =
    config.view (History.currentModel model.history)
        |> List.singleton
        |> view config model
        |> Html.div []



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



-- Session


type SessionSrc
    = Cache
    | Upload


emitCacheSession :
    (String -> Cmd (Msg model msg))
    -> (msg -> Encode.Value)
    -> Bool
    -> ( Model model msg, Cmd (Msg model msg) )
    -> ( Model model msg, Cmd (Msg model msg) )
emitCacheSession toCache encodeMsg isCachingSession ( model, cmd ) =
    if isCachingSession then
        Throttle.emit (toCache << encodeSession encodeMsg)
            UpdateCacheThrottle
            model.cacheThrottle
            model
            |> Tuple.mapBoth
                (\throttle -> { model | cacheThrottle = throttle })
                (\cacheCmd -> Cmd.batch [ cacheCmd, cmd ])

    else
        ( model, cmd )


encodeSession : (msg -> Encode.Value) -> Model model msg -> String
encodeSession encodeMsg model =
    Encode.encode 0 <|
        Encode.object
            [ ( "history", History.encode encodeMsg model.history )
            , ( "isViewInteractive", Encode.bool model.isViewInteractive )
            , ( "isModelVisible", Encode.bool model.isModelVisible )
            , ( "decodeStrategy", encodeDecodeStrategy model.decodeStrategy )
            , ( "description", Encode.string model.description )
            , ( "isCachingSession", Encode.bool model.isCachingSession )
            ]


sessionDecoder :
    (msg -> model -> model)
    -> Decoder msg
    -> DecodeStrategy
    -> ( model, Cmd msg )
    -> Decoder (Model model msg)
sessionDecoder update msgDecoder strategy ( model, cmd ) =
    Decode.map6
        (\history isViewInteractive decodeStrategy description isModelVisible isCachingSession ->
            { history = history
            , initCmd = cmd
            , isViewInteractive = isViewInteractive
            , decodeError = Nothing
            , decodeStrategy = decodeStrategy
            , description = description
            , isModelVisible = isModelVisible
            , cacheThrottle = Throttle.init
            , isCachingSession = isCachingSession
            }
        )
        (Decode.field "history" (toHistoryDecoder strategy update msgDecoder (History.init model)))
        (Decode.field "isViewInteractive" Decode.bool)
        (Decode.field "decodeStrategy" decodeStrategyDecoder)
        (Decode.field "description" Decode.string)
        (Decode.field "isModelVisible" Decode.bool)
        (Decode.field "isCachingSession" Decode.bool)



-- Helpers


view :
    { config
        | encodeMsg : msg -> Encode.Value
        , printModel : model -> String
        , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> List (Html msg)
    -> List (Html (Msg model msg))
view config model body =
    viewButton ResetApp "Restart"
        :: viewButton ToggleAppReplay
            (if History.isReplay model.history then
                "Continue"

             else
                "Pause"
            )
        :: viewButton DownloadSession "Download"
        :: viewButton SelectSession "Upload"
        :: viewButton ToggleCacheSession
            (if model.isCachingSession then
                "Disable Caching"

             else
                "Enable Caching"
            )
        :: viewButton ToggleDecodeStrategy
            (case model.decodeStrategy of
                NoErrors ->
                    "Decode messages with no errors"

                UntilError ->
                    "Decode messages until first error"

                SkipErrors ->
                    "Decode messages and skip errors"
            )
        :: viewButton ToggleViewInteractive
            (if model.isViewInteractive then
                "Enable View Events"

             else
                "Disable View Events"
            )
        :: viewButton ToggleModelVisibility
            (if model.isModelVisible then
                "Hide Model"

             else
                "Show Model"
            )
        :: viewStateCount model.history
        :: viewReplaySlider model.history
        :: viewDescription model.description
        :: viewDecodeError model.decodeError
        :: viewModel config.printModel model.history model.isModelVisible
        :: List.map (Html.map (updateAppIf model.isViewInteractive)) body


resultToTask : Result err ok -> Task err ok
resultToTask result =
    case result of
        Ok value ->
            Task.succeed value

        Err error ->
            Task.fail error


unwrapResult : (err -> ok) -> Result err ok -> ok
unwrapResult fromError result =
    case result of
        Ok value ->
            value

        Err error ->
            fromError error


updateAppIf : Bool -> msg -> Msg model msg
updateAppIf shouldUpdate =
    if shouldUpdate then
        UpdateApp View

    else
        always DoNothing


noCmd : model -> ( model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


ignoreCmd : (msg -> model -> ( model, Cmd msg )) -> msg -> model -> model
ignoreCmd update msg model =
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


viewDecodeError : Maybe ( SessionSrc, Decode.Error ) -> Html msg
viewDecodeError maybe =
    case maybe of
        Just ( src, error ) ->
            Html.div []
                [ case src of
                    Upload ->
                        Html.text "An upload failed with this error:\n"

                    Cache ->
                        Html.text "Failed to read from cache:\n"
                , Html.text (Decode.errorToString error)
                ]

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
