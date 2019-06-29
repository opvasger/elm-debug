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
import Helper
import History exposing (History)
import History.DecodeStrategy as DecodeStrategy exposing (DecodeStrategy)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task
import Throttle
import Time


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
    | DownloadSession Time.Posix
    | DownloadSessionWithDate
    | SelectSession
    | DecodeSession File
    | SessionDecoded (Result Decode.Error (Model model msg))
    | InputDescription String
    | UpdateCacheThrottle
    | InputTitle String


type alias Model model msg =
    { history : History model msg
    , initCmd : Cmd msg
    , isViewInteractive : Bool
    , isModelVisible : Bool
    , decodeStrategy : DecodeStrategy
    , decodeError : Maybe ( SessionSrc, Decode.Error )
    , description : String
    , cacheThrottle : Throttle.Model
    , title : String
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
                |> Maybe.map (Decode.decodeString (Decode.field "decodeStrategy" DecodeStrategy.decoder))
                |> Maybe.map (Result.withDefault DecodeStrategy.NoErrors)
                |> Maybe.withDefault DecodeStrategy.NoErrors

        decodeSession =
            config.init
                |> sessionDecoder (Helper.updateModel config.update) config.msgDecoder decodeStrategy

        toModel decodeError =
            { history = History.init (Tuple.first config.init)
            , initCmd = Tuple.second config.init
            , isViewInteractive = True
            , decodeError = Maybe.map (Tuple.pair Cache) decodeError
            , decodeStrategy = DecodeStrategy.UntilError
            , description = ""
            , isModelVisible = False
            , cacheThrottle = Throttle.init
            , title = defaultTitle
            }
    in
    config.fromCache
        |> Maybe.map (Helper.withoutCmd << Helper.unwrapResult (toModel << Just) << Decode.decodeString decodeSession)
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
            Helper.withoutCmd model

        UpdateApp src appMsg ->
            History.currentModel model.history
                |> config.update appMsg
                |> Tuple.second
                |> Cmd.map (UpdateApp Update)
                |> Tuple.pair { model | history = recordFromSrc src (Helper.updateModel config.update) appMsg model.history }
                |> emitCacheSession config.toCache config.encodeMsg

        ResetApp ->
            Cmd.map (UpdateApp Init) model.initCmd
                |> Tuple.pair
                    { model
                        | history = History.reset model.history
                        , decodeError = Nothing
                    }
                |> emitCacheSession config.toCache config.encodeMsg

        ReplayApp index ->
            { model | history = History.replay (Helper.updateModel config.update) index model.history }
                |> Helper.withoutCmd
                |> emitCacheSession config.toCache config.encodeMsg

        ToggleViewInteractive ->
            { model | isViewInteractive = not model.isViewInteractive }
                |> Helper.withoutCmd
                |> emitCacheSession config.toCache config.encodeMsg

        ToggleAppReplay ->
            { model | history = History.toggleReplay (Helper.updateModel config.update) model.history }
                |> Helper.withoutCmd
                |> emitCacheSession config.toCache config.encodeMsg

        DownloadSessionWithDate ->
            ( model, Task.perform DownloadSession Time.now )

        DownloadSession time ->
            let
                filename =
                    Helper.replaceEmptyWith defaultTitle model.title
                        ++ "."
                        ++ Helper.printUtcTime time
                        ++ ".json"
            in
            encodeSession config.encodeMsg model
                |> File.Download.string filename "application/json"
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
                        |> sessionDecoder (Helper.updateModel config.update) config.msgDecoder DecodeStrategy.NoErrors
            in
            File.toString file
                |> Task.map (Decode.decodeString decodeSession)
                |> Task.andThen Helper.resultToTask
                |> Task.attempt SessionDecoded
                |> Tuple.pair model

        SessionDecoded result ->
            case result of
                Ok sessionModel ->
                    Helper.withoutCmd sessionModel
                        |> emitCacheSession config.toCache config.encodeMsg

                Err error ->
                    Helper.withoutCmd { model | decodeError = Just ( Upload, error ) }

        ToggleDecodeStrategy ->
            { model | decodeStrategy = DecodeStrategy.loop model.decodeStrategy }
                |> Helper.withoutCmd
                |> emitCacheSession config.toCache config.encodeMsg

        ToggleModelVisibility ->
            { model | isModelVisible = not model.isModelVisible }
                |> Helper.withoutCmd
                |> emitCacheSession config.toCache config.encodeMsg

        InputDescription text ->
            { model | description = text }
                |> Helper.withoutCmd
                |> emitCacheSession config.toCache config.encodeMsg

        UpdateCacheThrottle ->
            Throttle.update (config.toCache << encodeSession config.encodeMsg)
                UpdateCacheThrottle
                model.cacheThrottle
                model
                |> Tuple.mapFirst (\cacheThrottle -> { model | cacheThrottle = cacheThrottle })

        InputTitle text ->
            { model | title = text }
                |> Helper.withoutCmd
                |> emitCacheSession config.toCache config.encodeMsg


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


defaultTitle : String
defaultTitle =
    "devtools-session"


emitCacheSession :
    (String -> Cmd (Msg model msg))
    -> (msg -> Encode.Value)
    -> ( Model model msg, Cmd (Msg model msg) )
    -> ( Model model msg, Cmd (Msg model msg) )
emitCacheSession toCache encodeMsg ( model, cmd ) =
    Throttle.emit (toCache << encodeSession encodeMsg)
        UpdateCacheThrottle
        model.cacheThrottle
        model
        |> Tuple.mapBoth
            (\throttle -> { model | cacheThrottle = throttle })
            (\cacheCmd -> Cmd.batch [ cacheCmd, cmd ])


encodeSession : (msg -> Encode.Value) -> Model model msg -> String
encodeSession encodeMsg model =
    Encode.encode 0 <|
        Encode.object
            [ ( "history", History.encode encodeMsg model.history )
            , ( "isViewInteractive", Encode.bool model.isViewInteractive )
            , ( "isModelVisible", Encode.bool model.isModelVisible )
            , ( "decodeStrategy", DecodeStrategy.encode model.decodeStrategy )
            , ( "description", Encode.string model.description )
            , ( "title", Encode.string model.title )
            ]


sessionDecoder :
    (msg -> model -> model)
    -> Decoder msg
    -> DecodeStrategy
    -> ( model, Cmd msg )
    -> Decoder (Model model msg)
sessionDecoder update msgDecoder strategy ( model, cmd ) =
    Decode.map6
        (\history isViewInteractive decodeStrategy description isModelVisible title ->
            { history = history
            , initCmd = cmd
            , isViewInteractive = isViewInteractive
            , decodeError = Nothing
            , decodeStrategy = decodeStrategy
            , description = description
            , isModelVisible = isModelVisible
            , cacheThrottle = Throttle.init
            , title = title
            }
        )
        (Decode.field "history" (DecodeStrategy.toHistoryDecoder strategy update msgDecoder model))
        (Decode.field "isViewInteractive" Decode.bool)
        (Decode.field "decodeStrategy" DecodeStrategy.decoder)
        (Decode.field "description" Decode.string)
        (Decode.field "isModelVisible" Decode.bool)
        (Decode.field "title" Decode.string)



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
        :: viewButton DownloadSessionWithDate "Download"
        :: viewButton SelectSession "Upload"
        :: viewButton ToggleDecodeStrategy
            (DecodeStrategy.toString model.decodeStrategy)
        :: viewButton ToggleViewInteractive
            (if model.isViewInteractive then
                "Disable View Events"

             else
                "Enable View Events"
            )
        :: viewButton ToggleModelVisibility
            (if model.isModelVisible then
                "Hide Model"

             else
                "Show Model"
            )
        :: viewStateCount model.history
        :: viewReplaySlider model.history
        :: viewTitle model.title
        :: viewDescription model.description
        :: viewDecodeError model.decodeError
        :: viewModel config.printModel model.history model.isModelVisible
        :: List.map (Html.map (updateAppIf model.isViewInteractive)) body


updateAppIf : Bool -> msg -> Msg model msg
updateAppIf shouldUpdate =
    if shouldUpdate then
        UpdateApp View

    else
        always DoNothing


viewButton : Msg model msg -> String -> Html (Msg model msg)
viewButton msg text =
    Html.button
        [ Html.Events.onClick msg
        ]
        [ Html.text text
        ]


viewTitle : String -> Html (Msg model msg)
viewTitle title =
    Html.input
        [ Html.Attributes.type_ "text"
        , Html.Attributes.placeholder defaultTitle
        , Html.Attributes.value title
        , Html.Events.onInput InputTitle
        ]
        []


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
                [ Html.text (String.fromInt (length + 1)) ]

            else
                [ Html.text (String.fromInt (currentIndex + 1))
                , Html.text "/"
                , Html.text (String.fromInt (length + 1))
                ]
    in
    Html.span [] children


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
