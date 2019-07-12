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
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , msgDecoder : Maybe (Decoder msg)
    , fromCache : Maybe String
    }
    -> ( Model model msg, Cmd (Msg model msg) )
init config =
    ( { history = History.init (Tuple.first config.init)
      , initCmd = Tuple.second config.init
      , decodeError = NoError
      , decodeStrategy = History.Decode.UntilError
      , cacheThrottle = Throttle.init
      , isModelVisible = False
      , description = ""
      , title = ""
      }
    , Cmd.none
    )


subscriptions :
    { update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , msgDecoder : Maybe (Decoder msg)
    }
    -> Model model msg
    -> Sub (Msg model msg)
subscriptions config model =
    Sub.none


update :
    { update : msg -> model -> ( model, Cmd msg )
    , toCache : Maybe (String -> Cmd (Msg model msg))
    , encodeMsg : Maybe (msg -> Encode.Value)
    , msgDecoder : Maybe (Decoder msg)
    }
    -> Msg model msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg model msg) )
update config msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        UpdateApp src appMsg ->
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
            ( { model
                | history =
                    History.restart model.history
              }
            , Cmd.map (UpdateApp FromInit) model.initCmd
            )

        ReplayApp index ->
            ( { model
                | history =
                    History.replay (dropCmd config.update)
                        index
                        model.history
              }
            , Cmd.none
            )

        ToggleAppReplay ->
            ( { model
                | history =
                    History.toggleReplay
                        (dropCmd config.update)
                        model.history
              }
            , Cmd.none
            )

        UseDecodeStrategy strategy ->
            ( { model | decodeStrategy = strategy }
            , Cmd.none
            )

        DownloadSessionWithDate ->
            Debug.todo "..."

        DownloadSession time ->
            case config.encodeMsg of
                Just encodeMsg ->
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
                                    History.Decode.NoErrors
                                    ( History.initialModel model.history
                                    , model.initCmd
                                    )
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
            ( { model | isModelVisible = not model.isModelVisible }
            , Cmd.none
            )

        InputTitle text ->
            ( { model | title = text }
            , Cmd.none
            )

        InputDescription text ->
            ( { model | description = text }
            , Cmd.none
            )


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
    -> History.Decode.Strategy
    -> ( model, Cmd msg )
    -> Decoder (Model model msg)
sessionDecoder updateApp msgDecoder strategy ( model, cmd ) =
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
        (Decode.field "decodeStrategy" History.Decode.strategyDecoder)
        (Decode.field "isModelVisible" Decode.bool)
        (Decode.field "title" Decode.string)
        (Decode.field "description" Decode.string)


view :
    { view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , encodeMsg : Maybe (msg -> Encode.Value)
    , encodeModel : Maybe (model -> Encode.Value)
    }
    -> Model model msg
    -> Browser.Document (Msg model msg)
view config model =
    let
        { title, body } =
            config.view
                (History.currentModel model.history)
    in
    { title = title
    , body =
        viewDevTools
            :: viewModel
            :: List.map (Html.map (UpdateApp FromView))
                body
    }


viewDevTools : Html (Msg model msg)
viewDevTools =
    Html.text "DevTools TODO"


viewModel : Html (Msg model msg)
viewModel =
    Html.text "Model TODO"



-- SessionDecodeError


type SessionDecodeError
    = NoError
    | CacheError Decode.Error
    | ImportError Decode.Error



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
