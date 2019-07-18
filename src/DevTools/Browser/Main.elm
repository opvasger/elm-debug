module DevTools.Browser.Main exposing
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
import DevTools.Browser.Element as Element
import DevTools.Browser.Element.Icon as Icon exposing (Icon)
import DevTools.Browser.Element.Range as Range
import DevTools.Browser.Window as Window
import File exposing (File)
import File.Download
import File.Select
import History exposing (History)
import History.Decode
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import JsonTree
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

    -- Report
    , title : String
    , description : String

    -- Layout
    , modelView : JsonTree.State
    , rangeInput : Range.Model
    , isModelVisible : Bool
    , window : Window.Model
    , focus : Maybe Icon
    , page : Page
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
    | LoadSession (Result Decode.Error (Model model msg))
    | UpdateCacheThrottle
      -- Report
    | InputTitle String
    | InputDescription String
      -- Layout
    | OpenPage Page
    | ToggleModelVisible
    | UpdateModelView JsonTree.State
    | UpdateWindow Window.Msg
    | UpdateFocus (Maybe Icon)
    | UpdateRange Range.Msg


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
            ( model
            , Cmd.map UpdateWindow (Tuple.second (Window.init True))
            )

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
      , window = Tuple.first (Window.init True)
      , focus = Nothing
      , modelView = JsonTree.defaultState
      , rangeInput = Range.init
      , page = Report
      }
    , Cmd.batch
        [ Cmd.map (UpdateApp FromInit) cmd
        , Cmd.map UpdateWindow (Tuple.second (Window.init True))
        ]
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
    Sub.batch
        [ Sub.map UpdateWindow
            (Window.subscriptions model.window)
        , if History.isReplay model.history then
            Range.subscriptions
                { onMove = ReplayApp
                , value = History.currentIndex model.history
                , max = History.length model.history
                }

          else
            Sub.map (UpdateApp FromSubs)
                (config.subscriptions
                    (History.currentModel model.history)
                )
        ]


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
                            (toSessionTitle time model.title)
                            "application/json"
                            (encodeSession encodeMsg model)
                        )

                _ ->
                    ( model, Cmd.none )

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
                        |> Task.attempt LoadSession
                    )

                _ ->
                    ( model, Cmd.none )

        LoadSession result ->
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
                    ( model, Cmd.none )

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
                    ( model, Cmd.none )

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

        ToggleModelVisible ->
            cache
                ( { model | isModelVisible = not model.isModelVisible }
                , Cmd.none
                )

        OpenPage page ->
            cache
                ( { model | page = page }
                , Cmd.none
                )

        UpdateWindow windowMsg ->
            cache
                ( { model | window = Window.update windowMsg model.window }
                , Cmd.none
                )

        UpdateFocus maybeIcon ->
            ( { model | focus = maybeIcon }
            , Cmd.none
            )

        UpdateModelView state ->
            ( { model | modelView = state }
            , Cmd.none
            )

        UpdateRange rangeMsg ->
            ( { model | rangeInput = Range.update rangeMsg model.rangeInput }
            , Cmd.none
            )


view :
    { config
        | view : model -> Browser.Document msg
        , update : msg -> model -> ( model, Cmd msg )
        , encodeMsg : Maybe (msg -> Encode.Value)
        , encodeModel : Maybe (model -> Encode.Value)
        , isImportEnabled : Bool
        , isCacheEnabled : Bool
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
        viewDevTools config model
            :: Window.ignoreSelectOnMove model.window (viewModel config model)
            :: List.map (Window.ignoreSelectOnMove model.window << Html.map (UpdateApp FromView))
                app.body
    }


viewDevTools :
    { config
        | encodeMsg : Maybe (msg -> Encode.Value)
        , encodeModel : Maybe (model -> Encode.Value)
        , isImportEnabled : Bool
        , isCacheEnabled : Bool
    }
    -> Model model msg
    -> Html (Msg model msg)
viewDevTools config model =
    Window.view UpdateWindow
        model.window
        { collapsed =
            \expandMsg ->
                [ viewModelButton config.encodeModel model
                , Element.viewDivider
                , viewRestartButton model
                , viewReplayRange model
                , viewToggleReplayButton model
                , Element.viewDivider
                , viewExpandButton expandMsg model
                ]
        , expanded =
            { head =
                \collapseMsg dismissMsg ->
                    [ viewModelButton config.encodeModel model
                    , viewDownloadButton config.encodeMsg model
                    , viewUploadButton config.isImportEnabled model
                    , Element.viewDivider
                    , Icon.viewReport
                        { focus = model.focus
                        , isActive = model.page == Report
                        , onClick = OpenPage Report
                        , onFocus = UpdateFocus
                        , title = "View report"
                        }
                    , Icon.viewSettings
                        { focus = model.focus
                        , isActive = model.page == Settings
                        , onClick = OpenPage Settings
                        , onFocus = UpdateFocus
                        , title = "View settings"
                        }
                    , Icon.viewMessages
                        { focus = model.focus
                        , isActive = model.page == Messages
                        , onClick = OpenPage Messages
                        , onFocus = UpdateFocus
                        , title = "View messages"
                        }
                    , Element.viewDivider
                    , viewDismissButton dismissMsg model
                    , viewCollapseButton collapseMsg model
                    ]
            , body =
                case model.page of
                    Report ->
                        viewReportPage model

                    Settings ->
                        viewSettingsPage model

                    Messages ->
                        viewMessagesPage model
            , foot =
                [ viewRestartButton model
                , viewReplayRange model
                , viewToggleReplayButton model
                ]
            }
        }


viewSettingsPage :
    config
    -> List (Html (Msg model msg))
viewSettingsPage model =
    []


viewMessagesPage :
    config
    -> List (Html (Msg model msg))
viewMessagesPage model =
    []


viewReportPage :
    { config | title : String, description : String }
    -> List (Html (Msg model msg))
viewReportPage model =
    [ Element.viewText
        { value = model.title
        , placeholder = defaultTitle
        , onInput = InputTitle
        }
    , Element.viewTextArea
        { value = model.description
        , placeholder = descriptionPlaceholder
        , onInput = InputDescription
        }
    ]


viewReplayRange :
    { config
        | rangeInput : Range.Model
        , history : History model msg
    }
    -> Html (Msg model msg)
viewReplayRange model =
    Range.view model.rangeInput
        { onUpdate = UpdateRange
        , onMove = ReplayApp
        , max = History.length model.history
        , value = History.currentIndex model.history
        }


viewToggleReplayButton :
    { config
        | history : History model msg
        , focus : Maybe Icon
    }
    -> Html (Msg model msg)
viewToggleReplayButton model =
    if History.isReplay model.history then
        Icon.viewPlay
            { focus = model.focus
            , onFocus = UpdateFocus
            , onClick = ToggleAppReplay
            , title = "Start the application"
            }

    else
        Icon.viewPause
            { focus = model.focus
            , onFocus = UpdateFocus
            , onClick = ToggleAppReplay
            , title = "Pause the application"
            }


viewRestartButton :
    { config | focus : Maybe Icon }
    -> Html (Msg model msg)
viewRestartButton model =
    Icon.viewRestart
        { focus = model.focus
        , onFocus = UpdateFocus
        , onClick = RestartApp
        , title = "Restart the application"
        }


viewDismissButton :
    Window.Msg
    -> { config | focus : Maybe Icon }
    -> Html (Msg model msg)
viewDismissButton dismissMsg model =
    Icon.viewDismiss
        { focus = model.focus
        , onFocus = UpdateFocus
        , onClick = UpdateWindow dismissMsg
        , title = "Dismiss the window"
        }


viewCollapseButton :
    Window.Msg
    -> { config | focus : Maybe Icon }
    -> Html (Msg model msg)
viewCollapseButton collapseMsg model =
    Icon.viewCollapse
        { focus = model.focus
        , onFocus = UpdateFocus
        , onClick = UpdateWindow collapseMsg
        , title = "Collapse the window"
        }


viewExpandButton :
    Window.Msg
    -> { config | focus : Maybe Icon }
    -> Html (Msg model msg)
viewExpandButton expandMsg model =
    Icon.viewExpand
        { focus = model.focus
        , onFocus = UpdateFocus
        , onClick = UpdateWindow expandMsg
        , title = "Expand the window"
        }


viewDownloadButton :
    Maybe (msg -> Encode.Value)
    -> { config | focus : Maybe Icon }
    -> Html (Msg model msg)
viewDownloadButton encodeMsg model =
    if encodeMsg /= Nothing then
        Icon.viewDownload
            { focus = model.focus
            , onFocus = UpdateFocus
            , onClick = DownloadSessionWithDate
            , title = "Download session"
            }

    else
        Element.viewNothing


viewUploadButton :
    Bool
    -> { config | focus : Maybe Icon }
    -> Html (Msg model msg)
viewUploadButton isImportEnabled model =
    if isImportEnabled then
        Icon.viewUpload
            { focus = model.focus
            , onFocus = UpdateFocus
            , onClick = SelectSessionFile
            , title = "Upload session"
            }

    else
        Element.viewNothing


viewModelButton :
    Maybe (model -> Encode.Value)
    -> { config | focus : Maybe Icon, isModelVisible : Bool }
    -> Html (Msg model msg)
viewModelButton encodeModel model =
    if encodeModel /= Nothing then
        Icon.viewModel
            { focus = model.focus
            , onFocus = UpdateFocus
            , onClick = ToggleModelVisible
            , title =
                if model.isModelVisible then
                    "Hide model"

                else
                    "View model"
            , isEnabled = model.isModelVisible
            }

    else
        Element.viewNothing


viewModel :
    { config
        | encodeModel : Maybe (model -> Encode.Value)
    }
    -> Model model msg
    -> Html (Msg model msg)
viewModel config model =
    case config.encodeModel of
        Just encodeModel ->
            Element.viewJson
                { isVisible = model.isModelVisible
                , value = History.currentModel model.history
                , encodeValue = encodeModel
                , onUpdate = UpdateModelView
                , state = model.modelView
                }

        Nothing ->
            Element.viewNothing



-- Page


type Page
    = Report
    | Settings
    | Messages


encodePage : Page -> Encode.Value
encodePage page =
    Encode.string <|
        case page of
            Report ->
                "report"

            Settings ->
                "settings"

            Messages ->
                "messages"


pageDecoder : Decoder Page
pageDecoder =
    Decode.andThen
        (\text ->
            case text of
                "report" ->
                    Decode.succeed Report

                "messages" ->
                    Decode.succeed Messages

                "settings" ->
                    Decode.succeed Settings

                _ ->
                    Decode.fail ("'" ++ text ++ "' is not of the page-type")
        )
        Decode.string



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
            , ( "window", Window.encode model.window )
            , ( "page", encodePage model.page )
            ]


sessionDecoder :
    (msg -> model -> model)
    -> Decoder msg
    -> ( model, Cmd msg )
    -> History.Decode.Strategy
    -> Decoder (Model model msg)
sessionDecoder updateApp msgDecoder ( model, cmd ) strategy =
    Decode.map7
        (\history decodeStrategy isModelVisible title description window page ->
            { history = history
            , initCmd = cmd
            , decodeError = NoError
            , decodeStrategy = decodeStrategy
            , isModelVisible = isModelVisible
            , cacheThrottle = Throttle.init
            , title = title
            , description = description
            , window = window
            , focus = Nothing
            , modelView = JsonTree.defaultState
            , rangeInput = Range.init
            , page = page
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
        (Decode.field "window" Window.decoder)
        (Decode.field "page" pageDecoder)


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


toSessionTitle : Time.Posix -> String -> String
toSessionTitle time title =
    let
        fileTitle =
            case title of
                "" ->
                    defaultTitle

                _ ->
                    title
    in
    fileTitle ++ "." ++ printUtcDate time ++ ".json"



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



-- Helpers


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


printUtcDate : Time.Posix -> String
printUtcDate time =
    List.foldl (++)
        ""
        [ Time.toYear Time.utc time
            |> String.fromInt
            |> String.right 2
        , "-"
        , Time.toMonth Time.utc time
            |> monthNumber
            |> String.fromInt
            |> String.padLeft 2 '0'
        , "-"
        , Time.toDay Time.utc time
            |> String.fromInt
            |> String.padLeft 2 '0'
        ]


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
