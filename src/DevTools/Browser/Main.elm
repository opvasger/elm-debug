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
import DevTools.Browser.Element as Element
import DevTools.Browser.Element.Icon as Icon exposing (Icon)
import DevTools.Browser.Element.Range as Range
import DevTools.Browser.Text as Text
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
    { history : History model msg
    , initCmd : Cmd msg
    , decodeStrategy : History.Decode.Strategy
    , decodeError : SessionDecodeError
    , cacheThrottle : Throttle.Model
    , title : String
    , comments : String
    , modelView : JsonTree.State
    , rangeInput : Range.Model
    , isModelVisible : Bool
    , window : Window.Model
    , focus : Maybe Icon
    , page : Page
    }


type Msg model msg
    = DoNothing
    | UpdateApp MsgSource msg
    | RestartApp
    | ReplayApp Int
    | ToggleAppReplay
    | UseDecodeStrategy History.Decode.Strategy
    | DownloadSessionWithDate
    | DownloadSession Time.Posix
    | SelectSessionFile
    | DecodeSession File
    | LoadSession (Result Decode.Error (Model model msg))
    | UpdateCacheThrottle
    | InputTitle String
    | InputComments String
    | OpenPage Page
    | ToggleModelVisible
    | UpdateModelView JsonTree.State
    | UpdateWindow Window.Msg
    | UpdateFocus (Maybe Icon)
    | UpdateRange Range.Msg


urlUpdate : msg -> Msg model msg
urlUpdate =
    UpdateApp FromUrl


init :
    { config
        | init : ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , msgDecoder : Maybe (Decoder msg)
        , fromCache : Maybe String
        , isExportEnabled : Bool
    }
    -> ( Model model msg, Cmd (Msg model msg) )
init config =
    case ( config.msgDecoder, config.fromCache ) of
        ( Just msgDecoder, Just fromCache ) ->
            initSession config msgDecoder fromCache

        _ ->
            initWith config NoError


initSession :
    { config
        | init : ( model, Cmd msg )
        , msgDecoder : Maybe (Decoder msg)
        , isExportEnabled : Bool
        , update : msg -> model -> ( model, Cmd msg )
    }
    -> Decoder msg
    -> String
    -> ( Model model msg, Cmd (Msg model msg) )
initSession config msgDecoder fromCache =
    let
        strategy =
            fromCache
                |> Decode.decodeString sessionStrategyDecoder
                |> Result.withDefault History.Decode.NoErrors

        decoder =
            sessionDecoder (dropCmd config.update)
                msgDecoder
                config.init
                strategy
    in
    case Decode.decodeString decoder fromCache of
        Ok model ->
            ( model
            , Cmd.map UpdateWindow (Tuple.second Window.init)
            )

        Err decodeError ->
            initWith config (CacheError decodeError)


initWith :
    { config
        | init : ( model, Cmd msg )
        , isExportEnabled : Bool
        , msgDecoder : Maybe (Decoder msg)
    }
    -> SessionDecodeError
    -> ( Model model msg, Cmd (Msg model msg) )
initWith config decodeError =
    ( { history = History.init (Tuple.first config.init)
      , initCmd = Tuple.second config.init
      , decodeError = decodeError
      , decodeStrategy = History.Decode.UntilError
      , cacheThrottle = Throttle.init
      , isModelVisible = False
      , comments = ""
      , title = ""
      , window = Tuple.first Window.init
      , focus = Nothing
      , modelView = JsonTree.defaultState
      , rangeInput = Range.init
      , page = Messages
      }
    , Cmd.batch
        [ Cmd.map (UpdateApp FromInit) (Tuple.second config.init)
        , Cmd.map UpdateWindow (Tuple.second Window.init)
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

        DoNothing ->
            ( model, Cmd.none )

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
                        , File.Download.string (Text.printFileName time model.title)
                            Text.jsonMimeType
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
                , File.Select.file [ Text.jsonMimeType ]
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
            if
                config.toCache
                    /= Nothing
                    && config.encodeMsg
                    /= Nothing
            then
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

            else
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

        InputComments text ->
            cache
                ( { model | comments = text }
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

        UpdateModelView state ->
            cache
                ( { model | modelView = state }
                , Cmd.none
                )

        UpdateRange rangeMsg ->
            cache
                ( { model | rangeInput = Range.update rangeMsg model.rangeInput }
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
        { collapsed = viewDevToolsCollapsed config model
        , expanded =
            { head = viewDevToolsHead config model
            , body = viewDevToolsBody config model
            , foot = viewDevToolsFoot model
            }
        }


viewDevToolsCollapsed :
    { config
        | encodeMsg : Maybe (msg -> Encode.Value)
        , encodeModel : Maybe (model -> Encode.Value)
        , isImportEnabled : Bool
        , isCacheEnabled : Bool
    }
    -> Model model msg
    -> Window.Msg
    -> List (Html (Msg model msg))
viewDevToolsCollapsed config model expandMsg =
    [ viewModelButton config.encodeModel model
    , if config.encodeModel /= Nothing then
        Element.viewDivider

      else
        Element.viewNothing
    , viewRestartButton model
    , viewReplayRange model
    , viewToggleReplayButton model
    , if
        config.encodeMsg
            /= Nothing
            || config.isCacheEnabled
            || config.isImportEnabled
      then
        Element.viewDivider

      else
        Element.viewNothing
    , if
        config.encodeMsg
            /= Nothing
            || config.isCacheEnabled
            || config.isImportEnabled
      then
        viewExpandButton expandMsg model

      else
        Element.viewNothing
    ]


viewDevToolsHead :
    { config
        | encodeMsg : Maybe (msg -> Encode.Value)
        , encodeModel : Maybe (model -> Encode.Value)
        , isImportEnabled : Bool
    }
    -> Model model msg
    -> Window.Msg
    -> Window.Msg
    -> List (Html (Msg model msg))
viewDevToolsHead config model collapseMsg dismissMsg =
    [ viewModelButton config.encodeModel model
    , viewDownloadButton config.encodeMsg model
    , viewUploadButton config.isImportEnabled model
    , Element.viewDivider
    , if config.encodeMsg /= Nothing then
        Icon.viewMessages
            { focus = model.focus
            , isActive = model.page == Messages
            , onClick = OpenPage Messages
            , onFocus = UpdateFocus
            , title = Text.messagesPageTitle
            }

      else
        Element.viewNothing
    , Icon.viewComments
        { focus = model.focus
        , isActive = model.page == Comments
        , onClick = OpenPage Comments
        , onFocus = UpdateFocus
        , title = Text.commentsPageTitle
        }
    , Icon.viewSettings
        { focus = model.focus
        , isActive = model.page == Settings
        , onClick = OpenPage Settings
        , onFocus = UpdateFocus
        , title = Text.settingsPageTitle
        }
    , Element.viewDivider
    , viewDismissButton dismissMsg model
    , viewCollapseButton collapseMsg model
    ]


viewDevToolsBody :
    { config
        | encodeMsg : Maybe (msg -> Encode.Value)
        , encodeModel : Maybe (model -> Encode.Value)
        , isImportEnabled : Bool
        , isCacheEnabled : Bool
    }
    -> Model model msg
    -> List (Html (Msg model msg))
viewDevToolsBody config model =
    case model.page of
        Comments ->
            viewCommentsPage
                { title = model.title
                , comments = model.comments
                , isExportEnabled = config.encodeMsg /= Nothing
                }

        Settings ->
            viewSettingsPage config.isCacheEnabled model

        Messages ->
            viewMessagesPage
                { history = model.history
                , encodeMsg = config.encodeMsg
                }


viewDevToolsFoot : Model model msg -> List (Html (Msg model msg))
viewDevToolsFoot model =
    [ viewRestartButton model
    , viewReplayRange model
    , viewToggleReplayButton model
    ]


viewDecodeStrategyInput :
    History.Decode.Strategy
    -> Int
    ->
        { config
            | decodeStrategy : History.Decode.Strategy
            , focus : Maybe Icon
        }
    -> Html (Msg model msg)
viewDecodeStrategyInput strategy key config =
    Icon.viewCheckbox
        { focus = config.focus
        , isActive = config.decodeStrategy == strategy
        , onClick = UseDecodeStrategy strategy
        , onFocus = UpdateFocus
        , title = Text.printStrategyDescription strategy
        , key = key
        }


viewSettingsPage :
    Bool
    ->
        { config
            | decodeStrategy : History.Decode.Strategy
            , focus : Maybe Icon
        }
    -> List (Html (Msg model msg))
viewSettingsPage isCacheEnabled model =
    if isCacheEnabled then
        [ Element.viewColumnWithTitle "Read from cache until the..."
            [ Element.viewRow
                [ Html.text "first unrecognized message."
                , viewDecodeStrategyInput History.Decode.UntilError 0 model
                ]
            , Element.viewRow
                [ Html.text "end, skipping unrecognized."
                , viewDecodeStrategyInput History.Decode.SkipErrors 1 model
                ]
            , Element.viewRow
                [ Html.text "end if all was recognized."
                , viewDecodeStrategyInput History.Decode.NoErrors 2 model
                ]
            ]
        ]

    else
        [ Element.viewTextInput
            { disabled = True
            , disabledPlaceholder = Text.noSettingsTitle
            , onInput = always DoNothing
            , placeholder = ""
            , value = ""
            }
        , Element.viewTextInputArea
            { disabled = True
            , disabledPlaceholder = Text.noSettingsPlaceholder
            , onInput = always DoNothing
            , placeholder = ""
            , value = ""
            }
        ]


viewMessagesPage :
    { config
        | history : History model msg
        , encodeMsg : Maybe (msg -> Encode.Value)
    }
    -> List (Html (Msg model msg))
viewMessagesPage config =
    case config.encodeMsg of
        Just encodeMsg ->
            List.map (mapPair (Element.viewMsg ReplayApp encodeMsg))
                (History.indexedRange 0 9 config.history)

        Nothing ->
            []


viewCommentsPage :
    { title : String
    , comments : String
    , isExportEnabled : Bool
    }
    -> List (Html (Msg model msg))
viewCommentsPage config =
    [ Element.viewTextInput
        { value = config.title
        , placeholder = Text.defaultSessionTitle
        , onInput = InputTitle
        , disabled = not config.isExportEnabled
        , disabledPlaceholder = Text.noCommentsTitle
        }
    , Element.viewTextInputArea
        { value = config.comments
        , placeholder = Text.commentsPlaceholder
        , onInput = InputComments
        , disabled = not config.isExportEnabled
        , disabledPlaceholder = Text.noCommentsPlaceholder
        }
    ]


viewReplayRange :
    { config
        | rangeInput : Range.Model
        , history : History model msg
    }
    -> Html (Msg model msg)
viewReplayRange model =
    let
        ( length, index ) =
            ( History.length model.history, History.currentIndex model.history )
    in
    Range.view model.rangeInput
        { onUpdate = UpdateRange
        , onMove = ReplayApp
        , max = length
        , value = index
        , title = Text.replayRangeTitle
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
            , title = Text.startAppTitle
            }

    else
        Icon.viewPause
            { focus = model.focus
            , onFocus = UpdateFocus
            , onClick = ToggleAppReplay
            , title = Text.pauseAppTitle
            }


viewRestartButton :
    { config | focus : Maybe Icon }
    -> Html (Msg model msg)
viewRestartButton model =
    Icon.viewRestart
        { focus = model.focus
        , onFocus = UpdateFocus
        , onClick = RestartApp
        , title = Text.restartAppTitle
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
        , title = Text.dissmissWindowTitle
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
        , title = Text.collapseWindowTitle
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
        , title = Text.expandWindowTitle
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
            , title = Text.downloadSessionTitle
            }

    else
        Element.viewNothing


viewUploadButton :
    Bool
    ->
        { config
            | focus : Maybe Icon
            , decodeError : SessionDecodeError
        }
    -> Html (Msg model msg)
viewUploadButton isImportEnabled model =
    if isImportEnabled then
        Icon.viewUpload
            { focus = model.focus
            , onFocus = UpdateFocus
            , onClick = SelectSessionFile
            , hasFailed = model.decodeError /= NoError
            , title = printSessionDecodeError Text.uploadSessionTitle model.decodeError
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
            , title = Text.printModelViewTitle model.isModelVisible
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
    = Comments
    | Settings
    | Messages


encodePage : Page -> Encode.Value
encodePage page =
    Encode.string <|
        case page of
            Comments ->
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
                    Decode.succeed Comments

                "messages" ->
                    Decode.succeed Messages

                "settings" ->
                    Decode.succeed Settings

                _ ->
                    Decode.fail ("'" ++ text ++ "' cannot be decoded into a page")
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
            , ( "comments", Encode.string model.comments )
            , ( "window", Window.encode model.window )
            , ( "page", encodePage model.page )
            , ( "modelView", JsonTree.stateToJson model.modelView )
            ]


sessionDecoder :
    (msg -> model -> model)
    -> Decoder msg
    -> ( model, Cmd msg )
    -> History.Decode.Strategy
    -> Decoder (Model model msg)
sessionDecoder updateApp msgDecoder ( model, cmd ) strategy =
    Decode.map8
        (\history decodeStrategy isModelVisible title comments window page modelView ->
            { history = history
            , initCmd = cmd
            , decodeError = NoError
            , decodeStrategy = decodeStrategy
            , isModelVisible = isModelVisible
            , cacheThrottle = Throttle.init
            , title = title
            , comments = comments
            , window = window
            , focus = Nothing
            , modelView = modelView
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
        (Decode.field "comments" Decode.string)
        (Decode.field "window" Window.decoder)
        (Decode.field "page" pageDecoder)
        (Decode.field "modelView" JsonTree.stateFromJson)


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


printSessionDecodeError : String -> SessionDecodeError -> String
printSessionDecodeError default error =
    case error of
        NoError ->
            default

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


mapPair : (a -> b -> c) -> ( a, b ) -> c
mapPair fn ( a, b ) =
    fn a b


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
