module DevTools.Browser.Program exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , urlUpdate
    , view
    )

import Browser
import Browser.Events
import File exposing (File)
import File.Download
import File.Select
import History exposing (History)
import History.Decode
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)
import Throttle
import Time



-- Program


type alias Model model msg =
    { -- App
      history : History model msg
    , initCmd : Cmd msg

    -- Session
    , decodeStrategy : History.Decode.Strategy
    , decodeError : SessionDecodeError
    , cacheThrottle : Throttle.Model

    -- Layout
    , isModelVisible : Bool

    -- Report
    , title : String
    , description : String
    }


type Msg model msg
    = DoNothing
      -- App
    | UpdateApp MsgSource msg
    | RestartApp
    | ReplayApp Int
    | ToggleAppReplay
      -- Session
    | UseDecodeStrategy History.Decode.Strategy
    | DownloadSessionWithDate
    | DownloadSession Time.Posix
    | SelectSessionFile
    | DecodeSession File
    | SessionDecoded (Result Decode.Error (Model model msg))
    | UpdateCacheThrottle
      -- Layout
    | ToggleModelVisible
      -- Report
    | InputTitle String
    | InputDescription String


defaultTitle : String
defaultTitle =
    "devtools-session"


descriptionPlaceholder : String
descriptionPlaceholder =
    """Take a moment to describe what you're doing!

 🐞 Did you encounter a bug
       you want to report?

 💭 Do you want to write a
       note before leaving?

 ⌗   Which Git-branch/commit
       is this session for?
"""


urlUpdate : msg -> Msg model msg
urlUpdate =
    UpdateApp FromUrl


init :
    { config
        | init : ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , msgDecoder : Maybe (Decoder msg)
        , fromCache : Maybe String
    }
    -> ( Model model msg, Cmd (Msg model msg) )
init config =
    case ( config.msgDecoder, config.fromCache ) of
        ( Just msgDecoder, Just fromCache ) ->
            initSession config.update msgDecoder fromCache config.init

        _ ->
            initWith NoError config.init


initSession :
    (msg -> model -> ( model, Cmd msg ))
    -> Decoder msg
    -> String
    -> ( model, Cmd msg )
    -> ( Model model msg, Cmd (Msg model msg) )
initSession updateApp msgDecoder fromCache initApp =
    let
        strategy =
            fromCache
                |> Decode.decodeString sessionStrategyDecoder
                |> Result.withDefault History.Decode.NoErrors

        decoder =
            sessionDecoder (dropCmd updateApp) msgDecoder initApp strategy
    in
    case Decode.decodeString decoder fromCache of
        Ok model ->
            ( model, Cmd.none )

        Err decodeError ->
            initWith (CacheError decodeError) initApp


initWith :
    SessionDecodeError
    -> ( model, Cmd msg )
    -> ( Model model msg, Cmd (Msg model msg) )
initWith decodeError ( model, cmd ) =
    ( { history = History.init model
      , initCmd = cmd
      , decodeError = decodeError
      , decodeStrategy = History.Decode.UntilError
      , cacheThrottle = Throttle.init
      , isModelVisible = False
      , description = ""
      , title = ""
      }
    , Cmd.map (UpdateApp FromInit) cmd
    )


subscriptions :
    { config
        | update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , msgDecoder : Maybe (Decoder msg)
    }
    -> Model model msg
    -> Sub (Msg model msg)
subscriptions config model =
    if History.isReplay model.history then
        Browser.Events.onKeyDown
            (replayKeyDecoder model.history)

    else
        Sub.map (UpdateApp FromSubs)
            (config.subscriptions
                (History.currentModel model.history)
            )


update :
    { config
        | update : msg -> model -> ( model, Cmd msg )
        , toCache : Maybe (String -> Cmd (Msg model msg))
        , encodeMsg : Maybe (msg -> Encode.Value)
        , msgDecoder : Maybe (Decoder msg)
    }
    -> Msg model msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg model msg) )
update config msg model =
    let
        cache =
            cacheSession
                { encodeMsg = config.encodeMsg
                , toCache = config.toCache
                }
    in
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        UpdateApp src appMsg ->
            cache
                ( { model
                    | history =
                        recordMsg src
                            (dropCmd config.update)
                            appMsg
                            model.history
                  }
                , History.currentModel model.history
                    |> config.update appMsg
                    |> Tuple.second
                    |> Cmd.map (UpdateApp FromUpdate)
                )

        RestartApp ->
            cache
                ( { model
                    | history =
                        History.restart model.history
                  }
                , Cmd.map (UpdateApp FromInit) model.initCmd
                )

        ReplayApp index ->
            cache
                ( { model
                    | history =
                        History.replay (dropCmd config.update)
                            index
                            model.history
                  }
                , Cmd.none
                )

        ToggleAppReplay ->
            cache
                ( { model
                    | history =
                        History.toggleReplay
                            (dropCmd config.update)
                            model.history
                  }
                , Cmd.none
                )

        UseDecodeStrategy strategy ->
            cache
                ( { model | decodeStrategy = strategy }
                , Cmd.none
                )

        DownloadSessionWithDate ->
            cache
                ( model
                , Task.perform DownloadSession Time.now
                )

        DownloadSession time ->
            case config.encodeMsg of
                Just encodeMsg ->
                    cache
                        ( model
                        , File.Download.string
                            (case model.title of
                                "" ->
                                    defaultTitle

                                title ->
                                    title
                            )
                            "application/json"
                            (encodeSession encodeMsg model)
                        )

                _ ->
                    Debug.todo "..."

        SelectSessionFile ->
            if model.decodeError /= NoError then
                ( { model | decodeError = NoError }
                , Cmd.none
                )

            else
                ( model
                , File.Select.file
                    [ "application/json" ]
                    DecodeSession
                )

        DecodeSession file ->
            case config.msgDecoder of
                Just msgDecoder ->
                    let
                        decodeSession =
                            Decode.decodeString <|
                                sessionDecoder (dropCmd config.update)
                                    msgDecoder
                                    ( History.initialModel model.history
                                    , model.initCmd
                                    )
                                    History.Decode.NoErrors
                    in
                    ( model
                    , File.toString file
                        |> Task.map decodeSession
                        |> Task.andThen resultToTask
                        |> Task.attempt SessionDecoded
                    )

                _ ->
                    Debug.todo "..."

        SessionDecoded result ->
            case ( config.toCache, config.encodeMsg ) of
                ( Just toCache, Just encodeMsg ) ->
                    case result of
                        Ok sessionModel ->
                            cache
                                ( sessionModel
                                , Cmd.none
                                )

                        Err error ->
                            ( { model | decodeError = ImportError error }
                            , Cmd.none
                            )

                _ ->
                    Debug.todo "..."

        UpdateCacheThrottle ->
            case ( config.toCache, config.encodeMsg ) of
                ( Just toCache, Just encodeMsg ) ->
                    Tuple.mapFirst
                        (\throttle -> { model | cacheThrottle = throttle })
                        (Throttle.update
                            (toCache << encodeSession encodeMsg)
                            UpdateCacheThrottle
                            model.cacheThrottle
                            model
                        )

                _ ->
                    Debug.todo "..."

        ToggleModelVisible ->
            cache
                ( { model | isModelVisible = not model.isModelVisible }
                , Cmd.none
                )

        InputTitle text ->
            cache
                ( { model | title = text }
                , Cmd.none
                )

        InputDescription text ->
            cache
                ( { model | description = text }
                , Cmd.none
                )


view :
    { config
        | view : model -> Browser.Document msg
        , update : msg -> model -> ( model, Cmd msg )
        , encodeMsg : Maybe (msg -> Encode.Value)
        , encodeModel : Maybe (model -> Encode.Value)
    }
    -> Model model msg
    -> Browser.Document (Msg model msg)
view config model =
    let
        app =
            config.view
                (History.currentModel model.history)
    in
    { title = app.title
    , body =
        viewDevTools
            :: viewModel
            :: List.map (Html.map (UpdateApp FromView))
                app.body
    }


viewDevTools : Html (Msg model msg)
viewDevTools =
    Html.text "DevTools TODO"


viewModel : Html (Msg model msg)
viewModel =
    Html.text "Model TODO"



-- Session


encodeSession : (msg -> Encode.Value) -> Model model msg -> String
encodeSession encodeMsg model =
    Encode.encode 0 <|
        Encode.object
            [ ( "history", History.encode encodeMsg model.history )
            , ( "isModelVisible", Encode.bool model.isModelVisible )
            , ( "decodeStrategy", History.Decode.encodeStrategy model.decodeStrategy )
            , ( "title", Encode.string model.title )
            , ( "description", Encode.string model.description )
            ]


sessionDecoder :
    (msg -> model -> model)
    -> Decoder msg
    -> ( model, Cmd msg )
    -> History.Decode.Strategy
    -> Decoder (Model model msg)
sessionDecoder updateApp msgDecoder ( model, cmd ) strategy =
    Decode.map5
        (\history decodeStrategy isModelVisible title description ->
            { history = history
            , initCmd = cmd
            , decodeError = NoError
            , decodeStrategy = decodeStrategy
            , isModelVisible = isModelVisible
            , cacheThrottle = Throttle.init
            , title = title
            , description = description
            }
        )
        (Decode.field "history"
            (History.Decode.fromStrategy
                strategy
                updateApp
                msgDecoder
                model
            )
        )
        sessionStrategyDecoder
        (Decode.field "isModelVisible" Decode.bool)
        (Decode.field "title" Decode.string)
        (Decode.field "description" Decode.string)


sessionStrategyDecoder : Decoder History.Decode.Strategy
sessionStrategyDecoder =
    Decode.field "decodeStrategy" History.Decode.strategyDecoder


cacheSession :
    { toCache : Maybe (String -> Cmd (Msg model msg))
    , encodeMsg : Maybe (msg -> Encode.Value)
    }
    -> ( Model model msg, Cmd (Msg model msg) )
    -> ( Model model msg, Cmd (Msg model msg) )
cacheSession config ( model, cmd ) =
    case ( config.toCache, config.encodeMsg ) of
        ( Just toCache, Just encodeMsg ) ->
            Tuple.mapBoth
                (\throttle -> { model | cacheThrottle = throttle })
                (\cacheCmd -> Cmd.batch [ cacheCmd, cmd ])
                (Throttle.emit (toCache << encodeSession encodeMsg)
                    UpdateCacheThrottle
                    model.cacheThrottle
                    model
                )

        _ ->
            ( model, cmd )



-- SessionDecodeError


type SessionDecodeError
    = NoError
    | CacheError Decode.Error
    | ImportError Decode.Error


printSessionDecodeError : SessionDecodeError -> String
printSessionDecodeError error =
    case error of
        NoError ->
            ""

        CacheError decodeError ->
            Decode.errorToString decodeError

        ImportError decodeError ->
            Decode.errorToString decodeError



-- MsgSource


type MsgSource
    = FromInit
    | FromUpdate
    | FromSubs
    | FromView
    | FromUrl


recordMsg :
    MsgSource
    -> (msg -> model -> model)
    -> msg
    -> History model msg
    -> History model msg
recordMsg src =
    case src of
        FromInit ->
            History.recordForever

        _ ->
            History.record



--


replayKeyDecoder : History model msg -> Decoder (Msg model msg)
replayKeyDecoder history =
    let
        replayBy diff =
            ReplayApp (History.currentIndex history + diff)

        toReplayMsg keyCode =
            case keyCode of
                37 ->
                    Decode.succeed (replayBy -1)

                39 ->
                    Decode.succeed (replayBy 1)

                _ ->
                    Decode.fail ""
    in
    Decode.andThen toReplayMsg
        (Decode.field "keyCode" Decode.int)


dropCmd :
    (msg -> model -> ( model, Cmd msg ))
    -> msg
    -> model
    -> model
dropCmd fn msg =
    Tuple.first << fn msg


resultToTask : Result e a -> Task e a
resultToTask result =
    case result of
        Ok value ->
            Task.succeed value

        Err error ->
            Task.fail error
