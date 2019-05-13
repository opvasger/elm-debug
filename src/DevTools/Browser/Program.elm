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
import Browser.Events
import Deque exposing (Deque)
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download as Download
import File.Select as Select
import History exposing (History)
import Html exposing (Html)
import Html.Events
import Icon
import Json.Decode as Jd
import Json.Encode as Je
import Position exposing (Position)
import Size exposing (Size)
import Task exposing (Task)


type alias Program flags model msg =
    Platform.Program flags (Model model msg) (Msg model msg)



-- Msg


type Msg model msg
    = DoNothing
    | UpdateApp MsgSrc msg
    | ReplayApp Int
    | RestartApp
    | ToggleModelShown
    | ToggleRewindOnHtmlMsg
    | ToggleReplay
    | UpdateMouse Mouse
    | ResizeViewport Size
    | MoveWindow Position
    | ImportModel (ImportModelMsg model msg)
    | ExportModel


type ImportModelMsg model msg
    = SelectFile
    | DecodeModel File
    | FileDecoded (Result Jd.Error (Model model msg))


mapUrlMsg : msg -> Msg model msg
mapUrlMsg =
    UpdateApp Url



-- MsgSrc


type MsgSrc
    = Init
    | Update
    | Subs
    | View
    | Url


updateHistory :
    MsgSrc
    -> (msg -> model -> model)
    -> msg
    -> History model msg
    -> History model msg
updateHistory msgSrc =
    case msgSrc of
        Init ->
            History.updateAndPersist

        _ ->
            History.update



-- Model


type alias Model model msg =
    { init : ( model, Cmd msg )
    , viewport : Size
    , window : Window
    , mouse : Mouse
    , history : History model msg
    , rewindOnHtmlMsg : Bool
    , isModelShown : Bool
    , cacheDecoding : CacheDecoding
    , importDecoding : ImportDecoding
    }


encodeModel : (msg -> Je.Value) -> Model model msg -> Je.Value
encodeModel encodeMsg { viewport, window, history, rewindOnHtmlMsg, isModelShown, cacheDecoding, importDecoding } =
    Je.object
        [ ( "viewport", Size.encode viewport )
        , ( "window", encodeWindow window )
        , ( "history", History.encode encodeMsg history )
        , ( "rewindOnHtmlMsg", Je.bool rewindOnHtmlMsg )
        , ( "isModelShown", Je.bool isModelShown )
        , ( "cacheDecoding", encodeCacheDecoding cacheDecoding )
        , ( "importDecoding", encodeImportDecoding importDecoding )
        ]


modelDecoder :
    (msg -> model -> model)
    -> ( model, Cmd msg )
    -> Jd.Decoder msg
    -> DecodingStrategy
    -> Jd.Decoder (Model model msg)
modelDecoder updateModel init msgDecoder strategy =
    Jd.map8 (Model init)
        (Jd.field "viewport" Size.decoder)
        (Jd.field "window" windowDecoder)
        (Jd.succeed Idle)
        (Jd.field "history" (strategyToDecoder strategy updateModel msgDecoder (Tuple.first init)))
        (Jd.field "rewindOnHtmlMsg" Jd.bool)
        (Jd.field "isModelShown" Jd.bool)
        (Jd.field "cacheDecoding" cacheDecodingDecoder)
        (Jd.field "importDecoding" importDecodingDecoder)



-- CacheDecoding


type alias CacheDecoding =
    { strategy : DecodingStrategy
    , error : Maybe Jd.Error
    , isEnabled : Bool
    }


encodeCacheDecoding : CacheDecoding -> Je.Value
encodeCacheDecoding { strategy, isEnabled } =
    Je.object
        [ ( "strategy", encodeDecodingStrategy strategy )
        , ( "isEnabled", Je.bool isEnabled )
        ]


cacheDecodingDecoder : Jd.Decoder CacheDecoding
cacheDecodingDecoder =
    Jd.map3 CacheDecoding
        (Jd.field "strategy" decodingStrategyDecoder)
        (Jd.succeed Nothing)
        (Jd.field "isEnabled" Jd.bool)



-- ImportDecoding


type alias ImportDecoding =
    { strategy : DecodingStrategy
    , error : Maybe Jd.Error
    }


encodeImportDecoding : ImportDecoding -> Je.Value
encodeImportDecoding { strategy } =
    Je.object
        [ ( "strategy", encodeDecodingStrategy strategy )
        ]


importDecodingDecoder : Jd.Decoder ImportDecoding
importDecodingDecoder =
    Jd.map2 ImportDecoding
        (Jd.field "strategy" decodingStrategyDecoder)
        (Jd.succeed Nothing)



-- DecodingStrategy


type DecodingStrategy
    = UntilError
    | SkipErrors
    | NoErrors


strategyToDecoder :
    DecodingStrategy
    -> (msg -> model -> model)
    -> Jd.Decoder msg
    -> model
    -> Jd.Decoder (History model msg)
strategyToDecoder strategy =
    case strategy of
        UntilError ->
            History.untilErrorDecoder

        SkipErrors ->
            History.skipErrorsDecoder

        NoErrors ->
            History.noErrorsDecoder


encodeDecodingStrategy : DecodingStrategy -> Je.Value
encodeDecodingStrategy strategy =
    Je.string
        (case strategy of
            UntilError ->
                "untilError"

            SkipErrors ->
                "skipErrors"

            NoErrors ->
                "noErrors"
        )


decodingStrategyDecoder : Jd.Decoder DecodingStrategy
decodingStrategyDecoder =
    Jd.andThen
        (\text ->
            case text of
                "untilError" ->
                    Jd.succeed UntilError

                "skipErrors" ->
                    Jd.succeed SkipErrors

                "noErrors" ->
                    Jd.succeed NoErrors

                _ ->
                    Jd.fail ("expected either 'untilError', 'skipErrors', or 'noErrors'. got " ++ text)
        )
        Jd.string



-- Window


type alias Window =
    { position : Position
    , body : Size
    }


moveWindow : Position -> Position -> Position -> Window -> Window
moveWindow from click to window =
    { window | position = Position.add from (Position.sub click to) }


encodeWindow : Window -> Je.Value
encodeWindow { position, body } =
    Je.object
        [ ( "position", Position.encode position )
        , ( "body", Size.encode body )
        ]


windowDecoder : Jd.Decoder Window
windowDecoder =
    Jd.map2 Window
        (Jd.field "position" Position.decoder)
        (Jd.field "body" Size.decoder)



-- Mouse


type Mouse
    = Idle
    | Target MouseTarget
    | Drag Position Position


type MouseTarget
    = NoTarget
    | ImportModelButton
    | ExportModelButton
    | ToggleModelShownButton
    | ToggleRewindOnHtmlMsgButton
    | ToggleReplayButton
    | RestartAppButton


mouseTarget : Mouse -> MouseTarget
mouseTarget mouse =
    case mouse of
        Target element ->
            element

        _ ->
            NoTarget


mouseSubscriptions : Mouse -> Sub (Msg model msg)
mouseSubscriptions mouse =
    case mouse of
        Drag _ _ ->
            Sub.batch
                [ Browser.Events.onMouseUp (Jd.succeed (UpdateMouse Idle))
                , Browser.Events.onMouseMove (Jd.map MoveWindow Position.mouseDecoder)
                ]

        _ ->
            Sub.none



-- Init


mapInit :
    { init : ( model, Cmd msg )
    , msgDecoder : Jd.Decoder msg
    , update : msg -> model -> ( model, Cmd msg )
    , fromCache : Maybe Je.Value
    }
    -> ( Model model msg, Cmd (Msg model msg) )
mapInit { fromCache, update, msgDecoder, init } =
    ( { init = init
      , history = History.init (Tuple.first init)
      , viewport = Size 800 600
      , rewindOnHtmlMsg = False
      , isModelShown = False
      , mouse = Idle
      , window =
            { position = Position 200 200
            , body = Size 200 300
            }
      , importDecoding =
            { strategy = UntilError
            , error = Nothing
            }
      , cacheDecoding =
            { strategy = NoErrors
            , error = Nothing
            , isEnabled = True
            }
      }
    , Cmd.batch
        [ Cmd.map (UpdateApp Init) (Tuple.second init)
        , Task.perform ResizeViewport Size.getViewport
        ]
    )



-- Subscriptions


mapSubscriptions :
    { msgDecoder : Jd.Decoder msg
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> Sub (Msg model msg)
mapSubscriptions { msgDecoder, subscriptions, update } { history, mouse } =
    Sub.batch
        [ Sub.map ResizeViewport Size.onResizeViewport
        , mouseSubscriptions mouse
        , appSubscriptions subscriptions (withoutCmd update) history
        ]


appSubscriptions :
    (model -> Sub msg)
    -> (msg -> model -> model)
    -> History model msg
    -> Sub (Msg model msg)
appSubscriptions subscriptions updateModel history =
    if History.isReplay history then
        Sub.none

    else
        Sub.map (UpdateApp Subs) (subscriptions (History.currentModel history))



-- Update


mapUpdate :
    { msgDecoder : Jd.Decoder msg
    , encodeMsg : msg -> Je.Value
    , update : msg -> model -> ( model, Cmd msg )
    , toCache : Je.Value -> Cmd (Msg model msg)
    }
    -> Msg model msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg model msg) )
mapUpdate { update, encodeMsg, msgDecoder } msg ({ importDecoding } as model) =
    case msg of
        DoNothing ->
            ( model
            , Cmd.none
            )

        UpdateApp msgSrc appMsg ->
            let
                ( _, appCmd ) =
                    update appMsg (History.currentModel model.history)
            in
            ( { model
                | history =
                    updateHistory msgSrc
                        (withoutCmd update)
                        appMsg
                        model.history
              }
            , Cmd.map (UpdateApp Update) appCmd
            )

        ResizeViewport size ->
            ( { model | viewport = size }
            , Cmd.none
            )

        ToggleModelShown ->
            ( { model | isModelShown = not model.isModelShown }
            , Cmd.none
            )

        ToggleRewindOnHtmlMsg ->
            ( { model | rewindOnHtmlMsg = not model.rewindOnHtmlMsg }
            , Cmd.none
            )

        RestartApp ->
            ( { model | history = History.init (Tuple.first model.init) }
            , Cmd.map (UpdateApp Init) (Tuple.second model.init)
            )

        ToggleReplay ->
            ( { model
                | history =
                    History.toggleReplay (withoutCmd update)
                        model.history
              }
            , Cmd.none
            )

        ReplayApp index ->
            ( { model | history = History.replay (withoutCmd update) index model.history }
            , Cmd.none
            )

        UpdateMouse mouse ->
            case ( mouse, model.mouse ) of
                ( Target _, Drag _ _ ) ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | mouse = mouse }
                    , Cmd.none
                    )

        MoveWindow to ->
            case model.mouse of
                Drag from click ->
                    ( { model | window = moveWindow from click to model.window }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        ExportModel ->
            ( model
            , Download.string
                -- TODO exported models need more useful names
                "devtools.json"
                "application/json"
                (Je.encode 0 (encodeModel encodeMsg model))
            )

        ImportModel imMsg ->
            case imMsg of
                SelectFile ->
                    ( model
                    , Select.file [ "application/json" ]
                        (ImportModel << DecodeModel)
                    )

                DecodeModel file ->
                    File.toString file
                        |> Task.map
                            (Jd.decodeString
                                (modelDecoder
                                    (withoutCmd update)
                                    model.init
                                    msgDecoder
                                    model.importDecoding.strategy
                                )
                            )
                        |> Task.andThen resultToTask
                        |> Task.attempt (ImportModel << FileDecoded)
                        |> Tuple.pair model

                FileDecoded result ->
                    case result of
                        Ok imported ->
                            ( imported, Cmd.none )

                        Err error ->
                            ( { model
                                | importDecoding =
                                    { importDecoding
                                        | error = Just error
                                    }
                              }
                            , Cmd.none
                            )


withoutCmd : (msg -> model -> ( model, Cmd msg )) -> msg -> model -> model
withoutCmd update msg model =
    Tuple.first (update msg model)


resultToTask : Result err ok -> Task err ok
resultToTask result =
    case result of
        Ok value ->
            Task.succeed value

        Err value ->
            Task.fail value



-- View


mapDocument :
    { encodeMsg : msg -> Je.Value
    , printModel : model -> String
    , viewApp : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> Browser.Document (Msg model msg)
mapDocument { printModel, encodeMsg, viewApp, update } model =
    let
        { title, body } =
            viewApp (History.currentModel model.history)
    in
    { title = title
    , body =
        view
            { printModel = printModel
            , encodeMsg = encodeMsg
            , appHtml = Html.div [] body
            , appModel = History.currentModel model.history
            }
            model
            :: []
    }


mapHtml :
    { encodeMsg : msg -> Je.Value
    , printModel : model -> String
    , viewApp : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> Html (Msg model msg)
mapHtml { printModel, encodeMsg, viewApp, update } model =
    view
        { printModel = printModel
        , encodeMsg = encodeMsg
        , appHtml = viewApp (History.currentModel model.history)
        , appModel = History.currentModel model.history
        }
        model


view :
    { encodeMsg : msg -> Je.Value
    , printModel : model -> String
    , appHtml : Html msg
    , appModel : model
    }
    -> Model model msg
    -> Html (Msg model msg)
view { encodeMsg, printModel, appHtml, appModel } model =
    El.layout
        [ El.inFront
            (El.el
                [ El.inFront (viewWindow model)
                ]
                (viewAppModel printModel model.isModelShown appModel)
            )
        ]
        (El.map
            (mapHtmlMsgs model.history model.rewindOnHtmlMsg)
            (El.html appHtml)
        )


mapHtmlMsgs : History model msg -> Bool -> msg -> Msg model msg
mapHtmlMsgs history rewindOnHtmlMsg =
    if not (History.isReplay history) || rewindOnHtmlMsg then
        UpdateApp View

    else
        always DoNothing


viewAppModel : (model -> String) -> Bool -> model -> Element (Msg model msg)
viewAppModel printModel isModelShown model =
    if isModelShown then
        El.el
            []
            (El.text (printModel model))

    else
        El.none


viewWindow : Model model msg -> Element (Msg model msg)
viewWindow { window, viewport, mouse, history, isModelShown, importDecoding } =
    let
        moveRight =
            clamp 0
                (viewport.width - window.body.width)
                window.position.left

        moveDown =
            clamp 0
                (viewport.height - window.body.height + 2 * barHeight)
                window.position.top
    in
    El.column
        [ El.width (El.px window.body.width)
        , El.height (El.px (window.body.height + 2 * barHeight))
        , El.moveRight (toFloat moveRight)
        , El.moveDown (toFloat moveDown)
        ]
        [ viewNavBar (UpdateMouse << Drag window.position)
            [ viewIconButton
                { isActive = isModelShown
                , target = ToggleModelShownButton
                , currentTarget = mouseTarget mouse
                , onClick = ToggleModelShown
                , onTarget = UpdateMouse << Target
                , icon = Icon.viewModel
                , error = Nothing
                }
            , viewIconButton
                { isActive = False
                , target = ExportModelButton
                , currentTarget = mouseTarget mouse
                , onClick = ExportModel
                , onTarget = UpdateMouse << Target
                , icon = Icon.viewExport
                , error = Nothing
                }
            , viewIconButton
                { isActive = False
                , target = ImportModelButton
                , currentTarget = mouseTarget mouse
                , onClick = ImportModel SelectFile
                , onTarget = UpdateMouse << Target
                , icon = Icon.viewImport
                , error = Maybe.map printImportError importDecoding.error
                }
            , viewDivider
            ]
        , viewBody
            { height = window.body.height
            }
            (El.column [] (List.map (El.text << Debug.toString) (Deque.toList (History.currentMsgs history))))
        , viewCtrlBar
            [ viewIconButton
                { isActive = False
                , target = RestartAppButton
                , currentTarget = mouseTarget mouse
                , onClick = RestartApp
                , onTarget = UpdateMouse << Target
                , icon = Icon.viewReset
                , error = Nothing
                }
            , viewSlider
                { value = History.currentIndex history
                , max = History.length history - 1
                , onChange = ReplayApp
                }
            , viewIconButton
                { isActive = not (History.isReplay history)
                , target = ToggleReplayButton
                , currentTarget = mouseTarget mouse
                , onClick = ToggleReplay
                , onTarget = UpdateMouse << Target
                , icon = Icon.viewPlay
                , error = Nothing
                }
            ]
        ]


printImportError : Jd.Error -> String
printImportError =
    -- TODO this message could be interesting to improve in the spirit of elms great error-messages
    Jd.errorToString


viewBody :
    { height : Int
    }
    -> Element msg
    -> Element msg
viewBody { height } =
    El.el
        [ El.height (El.px height)
        , Background.color white
        , Border.widthXY 1 0
        , Border.color borderGray
        , El.width El.fill
        , El.clip
        ]


viewSlider :
    { value : Int
    , max : Int
    , onChange : Int -> msg
    }
    -> Element msg
viewSlider { value, max, onChange } =
    Input.slider
        [ El.behindContent (viewSliderTrack max value)
        , El.height (El.px 18)
        , El.width El.fill
        ]
        { onChange = onChange << round
        , label = Input.labelHidden ""
        , min = 0
        , max = toFloat max
        , value = toFloat value
        , thumb = Input.defaultThumb
        , step = Just 1
        }


viewSliderTrack : Int -> Int -> Element msg
viewSliderTrack maxValue value =
    let
        ( leftPortion, rightPortion ) =
            if maxValue == 0 then
                ( 0, 1 )

            else
                ( value, maxValue - value )
    in
    El.row
        [ El.width El.fill
        , El.height (El.px 6)
        , El.centerY
        , Border.rounded 100
        , Border.color (El.rgb255 141 141 141)
        , Border.width 1
        ]
        [ El.el
            [ El.width (El.fillPortion leftPortion)
            , El.height El.fill
            , Background.color (El.rgb255 28 171 241)
            ]
            El.none
        , El.el
            [ El.width (El.fillPortion rightPortion)
            , El.height El.fill
            , Background.gradient
                { angle = 0
                , steps =
                    [ El.rgb255 194 194 194
                    , El.rgb255 163 163 163
                    ]
                }
            ]
            El.none
        ]


viewIconButton :
    { isActive : Bool
    , target : MouseTarget
    , currentTarget : MouseTarget
    , onClick : msg
    , onTarget : MouseTarget -> msg
    , icon : Icon.Style -> Element msg
    , error : Maybe String
    }
    -> Element msg
viewIconButton { isActive, target, currentTarget, onClick, onTarget, icon, error } =
    Input.checkbox
        [ El.width El.shrink
        , Events.onMouseEnter (onTarget target)
        , Events.onMouseLeave (onTarget NoTarget)
        ]
        { onChange = always onClick
        , checked = isActive
        , label = Input.labelHidden ""
        , icon =
            always
                (icon
                    (Icon.styleWith
                        { isActive = isActive
                        , isTarget = target == currentTarget
                        , error = error
                        }
                    )
                )
        }


viewDivider : Element msg
viewDivider =
    El.el
        [ El.paddingXY 3 0
        ]
        (El.el
            [ El.width (El.px 1)
            , El.height (El.px 16)
            , Background.color borderGray
            ]
            El.none
        )


viewNavBar : (Position -> msg) -> List (Element msg) -> Element msg
viewNavBar msg =
    El.row (onMouseDown (Jd.map msg Position.mouseDecoder) :: barStyle)


viewCtrlBar : List (Element msg) -> Element msg
viewCtrlBar =
    El.row barStyle


barStyle : List (El.Attribute msg)
barStyle =
    [ El.height (El.px barHeight)
    , Background.color backgroundGray
    , Border.color borderGray
    , El.width El.fill
    , El.paddingXY 5 0
    , Border.width 1
    , El.spacing 1
    ]


barHeight : Int
barHeight =
    27


white : El.Color
white =
    El.rgb255 255 255 255


borderGray : El.Color
borderGray =
    El.rgb255 211 211 211


backgroundGray : El.Color
backgroundGray =
    El.rgb255 243 243 243


onMouseDown : Jd.Decoder msg -> El.Attribute msg
onMouseDown =
    El.htmlAttribute
        << Html.Events.on "mousedown"
