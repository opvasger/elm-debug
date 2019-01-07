module DevTools exposing (Config, Program, doNothing, toDocument, toHtml, toInit, toMsg, toSubscriptions, toUpdate)

import Browser
import Browser.Dom as Bd
import Browser.Events as Be
import File as F exposing (File)
import File.Download as Fd
import File.Select as Fs
import Html as H exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import Html.Keyed as Hk
import Html.Lazy as Hl
import Json.Decode as Jd
import Json.Encode as Je
import Position as P exposing (Position)
import Size exposing (Size)
import Svg as S exposing (Svg)
import Svg.Attributes as Sa
import Svg.Events as Se
import Task exposing (Task)
import Time
import Tuple
import ZipList as Zl exposing (ZipList)


type Page
    = Notes
    | Updates


type DragEvent
    = Start
    | To Position
    | Stop


type Hoverable
    = OverlayButton
    | DragButton
    | DismissButton
    | LayoutButton
    | ImportButton
    | ExportButton
    | UpdateSlider
    | NavigationButtonFor Page
    | UpdateButtonAt Int
    | SubscribeButton
    | None


type alias Model model msg =
    { updates : ZipList ( Maybe msg, model )
    , importError : Maybe Jd.Error
    , notes : String
    , sessionTitle : String
    , isModelOverlayed : Bool
    , hoveredElement : Hoverable
    , viewportSize : Size
    , position : Position
    , isDragging : Bool
    , isSubscribed : Bool
    , layout : Layout
    , page : Page
    }


type Msg msg
    = UpdateWith msg
    | ToggleLayout
    | ToggleModelOverlay
    | ToggleSubscriptions
    | SelectFile
    | Drag DragEvent
    | Hover Hoverable
    | ResizeViewport Size
    | SelectUpdateAt Int
    | SelectPage Page
    | FileSelected File
    | ImportSession String
    | InputNotes String
    | InputSessionTitle String
    | ExportUpdates
    | Dismiss
    | DoNothing


type Layout
    = Collapsed
    | Expanded


type alias Config model msg =
    { printModel : model -> String
    , encodeMsg : msg -> Je.Value
    , msgDecoder : Jd.Decoder msg
    , output : Je.Value -> Cmd (Msg msg)
    }


type alias Program flags model msg =
    Platform.Program flags (Model model msg) (Msg msg)


doNothing : Msg msg
doNothing =
    DoNothing


toDocument :
    { encodeMsg : msg -> Je.Value
    , printModel : model -> String
    , view : model -> Browser.Document msg
    }
    -> Model model msg
    -> Browser.Document (Msg msg)
toDocument { printModel, encodeMsg, view } model =
    let
        { title, body } =
            view (Tuple.second model.updates.current)
    in
    { title = title
    , body =
        [ Hl.lazy3
            selectable
            (not model.isDragging)
            [ toCursorStyle model.isDragging model.hoveredElement ]
            (Hl.lazy2 viewDevTools encodeMsg model
                :: Hl.lazy2 viewIf model.isModelOverlayed (Hl.lazy2 viewOverlay model.viewportSize (formatPrintedModel (printModel (Tuple.second model.updates.current))))
                :: List.map (H.map UpdateWith) body
            )
        ]
    }


toHtml :
    { encodeMsg : msg -> Je.Value
    , printModel : model -> String
    , view : model -> Html msg
    }
    -> Model model msg
    -> Html (Msg msg)
toHtml { printModel, encodeMsg, view } model =
    Hl.lazy3
        selectable
        (not model.isDragging)
        [ toCursorStyle model.isDragging model.hoveredElement
        ]
        [ Hl.lazy2 viewDevTools encodeMsg model
        , Hl.lazy2 viewIf model.isModelOverlayed (Hl.lazy2 viewOverlay model.viewportSize (formatPrintedModel (printModel (Tuple.second model.updates.current))))
        , H.map UpdateWith (Hl.lazy view (Tuple.second model.updates.current))
        ]


toInit :
    { modelCmdPair : ( model, Cmd msg )
    , flags : Jd.Value
    , msgDecoder : Jd.Decoder msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> ( Model model msg, Cmd (Msg msg) )
toInit { update, msgDecoder, flags, modelCmdPair } =
    case Jd.decodeValue (sessionDecoder msgDecoder (Tuple.first modelCmdPair) Size.init) flags of
        Ok ( session, msgZl ) ->
            let
                ( index, msgs ) =
                    case msgZl of
                        Just zl ->
                            ( Zl.currentIndex zl, Zl.toList zl )

                        Nothing ->
                            ( 0, [] )
            in
            updateWith
                update
                (List.filterMap identity msgs)
                index
                session.isSubscribed
                ( session
                , Cmd.batch
                    [ Task.perform ResizeViewport (Task.map Size.fromViewport Bd.getViewport)
                    , if index == 0 then
                        Cmd.map UpdateWith (Tuple.second modelCmdPair)

                      else
                        Cmd.none
                    ]
                )

        Err importError ->
            ( { updates = Zl.singleton ( Nothing, Tuple.first modelCmdPair )
              , position = P.init
              , viewportSize = Size.init
              , layout = Collapsed
              , isModelOverlayed = False
              , isSubscribed = True
              , notes = ""
              , page = Updates
              , isDragging = False
              , hoveredElement = None
              , importError = Just importError
              , sessionTitle = ""
              }
            , Cmd.batch
                [ Cmd.map UpdateWith (Tuple.second modelCmdPair)
                , Task.perform ResizeViewport (Task.map Size.fromViewport Bd.getViewport)
                ]
            )


toMsg : msg -> Msg msg
toMsg =
    UpdateWith


toSubscriptions :
    { msgDecoder : Jd.Decoder msg
    , subscriptions : model -> Sub msg
    }
    -> Model model msg
    -> Sub (Msg msg)
toSubscriptions { msgDecoder, subscriptions } { updates, position, isDragging, isSubscribed } =
    Sub.batch
        [ Be.onResize (Size.mapFromInts ResizeViewport)
        , subscribeIf isSubscribed <|
            Sub.map UpdateWith (subscriptions (Tuple.second updates.current))
        , subscribeIf isDragging <|
            Sub.batch
                [ Be.onMouseMove (Jd.map (Drag << To) (P.mouseMoveDecoder position))
                , Be.onMouseUp (Jd.succeed (Drag Stop))
                ]
        ]


toUpdate :
    { msgDecoder : Jd.Decoder msg
    , encodeMsg : msg -> Je.Value
    , update : msg -> model -> ( model, Cmd msg )
    , output : Je.Value -> Cmd (Msg msg)
    }
    -> Msg msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg msg) )
toUpdate { msgDecoder, encodeMsg, update, output } msg model =
    let
        save =
            saveSession output encodeMsg
    in
    case msg of
        UpdateWith updateMsg ->
            let
                ( updateModel, updateCmd ) =
                    update updateMsg (Tuple.second model.updates.current)
            in
            save
                ( { model | updates = Zl.dropHeads (Zl.insert ( Just updateMsg, updateModel ) model.updates) }
                , Cmd.map UpdateWith updateCmd
                )

        SelectUpdateAt index ->
            save
                ( { model
                    | updates = Zl.toIndex index model.updates
                    , isSubscribed = index == Zl.length model.updates - 1
                  }
                , Cmd.none
                )

        SelectPage page ->
            save
                ( { model | page = page }, Cmd.none )

        FileSelected file ->
            ( model, Task.perform ImportSession (F.toString file) )

        ToggleModelOverlay ->
            save
                ( { model
                    | isModelOverlayed = not model.isModelOverlayed
                  }
                , Cmd.none
                )

        InputSessionTitle sessionTitle ->
            save ( { model | sessionTitle = String.trim sessionTitle }, Cmd.none )

        ToggleLayout ->
            let
                position =
                    P.add model.position <|
                        P.sub
                            (P.fromSize (layoutToSize (toggleLayout model.layout)))
                            (P.fromSize (layoutToSize model.layout))
            in
            save
                ( { model
                    | layout = toggleLayout model.layout
                    , isModelOverlayed = model.isModelOverlayed && model.layout == Collapsed
                    , position = clampToViewport (layoutToSize (toggleLayout model.layout)) model.viewportSize position
                  }
                , Cmd.none
                )

        SelectFile ->
            ( { model | importError = Nothing }
            , Fs.file [ jsonMimeType ] FileSelected
            )

        ResizeViewport viewportSize ->
            let
                layoutSize =
                    layoutToSize model.layout
            in
            save
                ( { model
                    | viewportSize = viewportSize
                    , position = clampToViewport layoutSize viewportSize model.position
                  }
                , Cmd.none
                )

        Hover hoveredElement ->
            ( { model | hoveredElement = hoveredElement }, Cmd.none )

        Dismiss ->
            save
                ( { model
                    | position =
                        clampToViewport
                            (layoutToSize model.layout)
                            model.viewportSize
                            { top = 0
                            , left = model.viewportSize.width
                            }
                  }
                , Cmd.none
                )

        InputNotes notes ->
            save
                ( { model | notes = String.join "\n\n" (String.split "\n\n\n" (String.trimLeft notes)) }, Cmd.none )

        ToggleSubscriptions ->
            save
                ( { model | isSubscribed = not model.isSubscribed }, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )

        ExportUpdates ->
            case Zl.filterMap Tuple.first model.updates of
                Nothing ->
                    ( model, Cmd.none )

                Just msgZl ->
                    ( model
                    , Fd.string (withDefaultSessionTitle model.sessionTitle) jsonMimeType (Je.encode 0 (encodeSession encodeMsg model))
                    )

        Drag Start ->
            ( { model | isDragging = True }, Cmd.none )

        Drag (To position) ->
            ( { model
                | position =
                    clampToViewport
                        (layoutToSize model.layout)
                        model.viewportSize
                        (toRelativeDragButtonPosition position model.layout)
              }
            , Cmd.none
            )

        Drag Stop ->
            save
                ( { model | isDragging = False }, Cmd.none )

        ImportSession text ->
            case Jd.decodeString (sessionDecoder msgDecoder (Tuple.second (Zl.tail model.updates)) model.viewportSize) text of
                Ok ( session, msgZl ) ->
                    let
                        ( index, msgs ) =
                            case msgZl of
                                Just zl ->
                                    ( Zl.currentIndex zl, Zl.toList zl )

                                Nothing ->
                                    ( 0, [] )
                    in
                    save
                        (updateWith update (List.filterMap identity msgs) index session.isSubscribed ( { session | isSubscribed = False }, Cmd.none ))

                Err err ->
                    ( { model | importError = Just err }, Cmd.none )


viewDevTools : (msg -> Je.Value) -> Model model msg -> Html (Msg msg)
viewDevTools encodeMsg model =
    let
        isExpanded =
            model.layout == Expanded

        layoutSize =
            layoutToSize model.layout

        updateCount =
            Zl.length model.updates

        toUpdateStringTuple index ( msg, _ ) =
            ( index
            , Maybe.withDefault "Init" (Maybe.map (Je.encode 0 << encodeMsg) msg)
            )
    in
    H.div
        [ Ha.style "position" "fixed"
        , Ha.style "z-index" "2147483647"
        , Ha.style "font-family" "system-ui"
        , Ha.style "background-color" "white"
        , Ha.style "border" "1px solid #d3d3d3"
        , Ha.style "top" (toPx model.position.top)
        , Ha.style "left" (toPx model.position.left)
        , Ha.style "width" (toPx (.width layoutSize))
        , Ha.style "height" (toPx (.height layoutSize))
        , onRightClick DoNothing
        ]
        [ viewIf isExpanded <|
            viewControls <|
                [ Hl.lazy2 viewOverlayButton model.hoveredElement model.isModelOverlayed
                , Hl.lazy2 viewSubscribeButton model.hoveredElement model.isSubscribed
                , Hl.lazy2 viewExportButton model.hoveredElement (updateCount > 1)
                , Hl.lazy2 viewImportButton model.hoveredElement model.importError
                , viewDivider
                , Hl.lazy4 viewNavigationPage 0 Updates model.page model.hoveredElement
                , Hl.lazy4 viewNavigationPage 1 Notes model.page model.hoveredElement
                , Hl.lazy viewNavigationUnderline model.page
                ]
        , viewIf isExpanded <|
            Hl.lazy viewPage
                { currentIndex = Zl.currentIndex model.updates
                , currentHover = model.hoveredElement
                , isSubscribed = model.isSubscribed
                , layoutSize = layoutSize
                , page = model.page
                , updates = Zl.toList (Zl.trim 10 (Zl.indexedMap toUpdateStringTuple model.updates))
                , notes = model.notes
                , sessionTitle = model.sessionTitle
                }
        , viewControls
            [ Hl.lazy2 viewSlider updateCount (Zl.currentIndex model.updates)
            , viewDivider
            , Hl.lazy2 viewDragButton model.hoveredElement model.isDragging
            , Hl.lazy3 viewDismissButton model.hoveredElement layoutSize model.position
            , Hl.lazy2 viewLayoutButton model.hoveredElement model.layout
            ]
        ]


saveSession : (Je.Value -> Cmd (Msg msg)) -> (msg -> Je.Value) -> ( Model model msg, Cmd (Msg msg) ) -> ( Model model msg, Cmd (Msg msg) )
saveSession output encodeMsg ( model, cmd ) =
    ( model
    , Cmd.batch
        [ cmd
        , output (encodeSession encodeMsg model)
        ]
    )


updateWith : (msg -> model -> ( model, Cmd msg )) -> List msg -> Int -> Bool -> ( Model model msg, Cmd (Msg msg) ) -> ( Model model msg, Cmd (Msg msg) )
updateWith update msgs index isSubscribed ( model, cmd ) =
    case msgs of
        updateMsg :: updateMsgs ->
            let
                ( updateModel, updateCmd ) =
                    update updateMsg (Tuple.second model.updates.current)
            in
            updateWith update
                updateMsgs
                index
                isSubscribed
                ( { model | updates = Zl.dropHeads (Zl.insert ( Just updateMsg, updateModel ) model.updates) }
                , Cmd.batch
                    [ cmd
                    , Cmd.map UpdateWith updateCmd
                    ]
                )

        [] ->
            ( { model
                | updates = Zl.toIndex index model.updates
                , isSubscribed = index == Zl.length model.updates - 1 && isSubscribed
              }
            , cmd
            )


formatPrintedModel : String -> String
formatPrintedModel =
    -- TODO
    identity


withDefaultSessionTitle : String -> String
withDefaultSessionTitle sessionTitle =
    case sessionTitle of
        "" ->
            "elm-dev-tools"

        _ ->
            sessionTitle


pageToJson : Page -> Je.Value
pageToJson page =
    case page of
        Updates ->
            Je.string "updates"

        Notes ->
            Je.string "notes"


pageFromJson : Jd.Decoder Page
pageFromJson =
    Jd.andThen
        (\str ->
            case str of
                "updates" ->
                    Jd.succeed Updates

                "notes" ->
                    Jd.succeed Notes

                _ ->
                    Jd.fail ("Failed to decode Page from '" ++ str ++ "'")
        )
        Jd.string


layoutToJson : Layout -> Je.Value
layoutToJson layout =
    case layout of
        Collapsed ->
            Je.string "collapsed"

        Expanded ->
            Je.string "expanded"


layoutFromJson : Jd.Decoder Layout
layoutFromJson =
    Jd.andThen
        (\str ->
            case str of
                "collapsed" ->
                    Jd.succeed Collapsed

                "expanded" ->
                    Jd.succeed Expanded

                _ ->
                    Jd.fail ("Failed to decode Page from '" ++ str ++ "'")
        )
        Jd.string


jsonEncodeMaybe : (value -> Je.Value) -> Maybe value -> Je.Value
jsonEncodeMaybe encodeValue maybeValue =
    case maybeValue of
        Just value ->
            encodeValue value

        Nothing ->
            Je.null


encodeSession : (msg -> Je.Value) -> Model model msg -> Je.Value
encodeSession encodeMsg { updates, isModelOverlayed, position, layout, page, viewportSize, isSubscribed, notes, sessionTitle } =
    let
        encodeSessionHelper =
            Je.object
                [ ( "updates", Zl.jsonEncode (jsonEncodeMaybe encodeMsg) (Zl.map Tuple.first updates) )
                , ( "isModelOverlayed", Je.bool isModelOverlayed )
                , ( "isSubscribed", Je.bool isSubscribed )
                , ( "position", P.jsonEncode position )
                , ( "notes", Je.string notes )
                , ( "layout", layoutToJson layout )
                , ( "page", pageToJson page )
                , ( "sessionTitle", Je.string sessionTitle )
                ]
    in
    Je.object [ ( "devTools", Je.string (Je.encode 0 encodeSessionHelper) ) ]


sessionDecoder : Jd.Decoder msg -> model -> Size -> Jd.Decoder ( Model model msg, Maybe (ZipList (Maybe msg)) )
sessionDecoder decodeMsg model viewportSize =
    let
        sessionDecoderHelper =
            Jd.map8
                (\updates isModelOverlayed isSubscribed position layout page notes sessionTitle ->
                    ( { updates = Zl.singleton ( Nothing, model )
                      , isModelOverlayed = isModelOverlayed
                      , viewportSize = viewportSize
                      , notes = notes
                      , importError = Nothing
                      , position = position
                      , isDragging = False
                      , isSubscribed = isSubscribed
                      , hoveredElement = None
                      , layout = layout
                      , page = page
                      , sessionTitle = sessionTitle
                      }
                    , updates
                    )
                )
                (Jd.field "updates" (Jd.maybe (Zl.jsonDecoder (Jd.maybe decodeMsg))))
                (Jd.field "isModelOverlayed" Jd.bool)
                (Jd.field "isSubscribed" Jd.bool)
                (Jd.field "position" P.jsonDecoder)
                (Jd.field "layout" layoutFromJson)
                (Jd.field "page" pageFromJson)
                (Jd.field "notes" Jd.string)
                (Jd.field "sessionTitle" Jd.string)
    in
    Jd.field "devTools" (nestedJsonDecoder sessionDecoderHelper)


nestedJsonDecoder : Jd.Decoder value -> Jd.Decoder value
nestedJsonDecoder decoder =
    let
        nestedJsonDecoderHelper text =
            case Jd.decodeString decoder text of
                Ok session ->
                    Jd.succeed session

                Err error ->
                    Jd.fail (Jd.errorToString error)
    in
    Jd.andThen nestedJsonDecoderHelper Jd.string


jsonMimeType : String
jsonMimeType =
    "application/json"


fit : Int -> String -> String
fit maxLength text =
    if String.length text > maxLength then
        String.left (maxLength - 3) text ++ "..."

    else
        text


clampToViewport : Size -> Size -> Position -> Position
clampToViewport size viewportSize position =
    P.clamp (Position 0 0) viewportSize size position


toPx : Int -> String
toPx n =
    String.fromInt n ++ "px"


viewIf : Bool -> Html msg -> Html msg
viewIf isVisible html =
    if isVisible then
        html

    else
        H.text ""


subscribeIf : Bool -> Sub msg -> Sub msg
subscribeIf isSubscribing subscribe =
    if isSubscribing then
        subscribe

    else
        Sub.none


listJoin : (a -> a -> a) -> a -> List a -> a
listJoin add unit list =
    case list of
        head :: tails ->
            List.foldl add head tails

        [] ->
            unit


selectable : Bool -> List (H.Attribute msg) -> List (Html msg) -> Html msg
selectable isSelectable attributes =
    H.div
        (if isSelectable then
            attributes

         else
            attributes
                ++ [ Ha.style "-webkit-touch-callout" "none"
                   , Ha.style "-webkit-user-select" "none"
                   , Ha.style "-khtml-user-select" "none"
                   , Ha.style "-moz-user-select" "none"
                   , Ha.style "-ms-user-select" "none"
                   , Ha.style "user-select" "none"
                   ]
        )


onRightClick : msg -> H.Attribute msg
onRightClick msg =
    He.preventDefaultOn "contextmenu" (Jd.succeed ( msg, True ))


isJust : Maybe a -> Bool
isJust maybe =
    maybe /= Nothing


isOdd : Int -> Bool
isOdd n =
    modBy 2 n == 1


toggleLayout : Layout -> Layout
toggleLayout layout =
    case layout of
        Collapsed ->
            Expanded

        Expanded ->
            Collapsed


toRelativeDragButtonPosition : Position -> Layout -> Position
toRelativeDragButtonPosition { top, left } layout =
    { left = left - 136
    , top =
        case layout of
            Expanded ->
                top - 210

            Collapsed ->
                top - 10
    }


pageToOverflow : Page -> String
pageToOverflow page =
    case page of
        Notes ->
            "hidden scroll"

        Updates ->
            "hidden"


toCursorStyle : Bool -> Hoverable -> H.Attribute msg
toCursorStyle isDragging currentHover =
    let
        cursor =
            if isDragging then
                "grabbing"

            else
                case currentHover of
                    None ->
                        "unset"

                    DragButton ->
                        "grab"

                    UpdateSlider ->
                        "grab"

                    _ ->
                        "pointer"
    in
    Ha.style "cursor" cursor


layoutToSize : Layout -> Size
layoutToSize layout =
    case layout of
        Collapsed ->
            { height = 20, width = 179 }

        Expanded ->
            { height = 222, width = 179 }


viewDivider : Html msg
viewDivider =
    H.div
        [ Ha.style "border-left" "1px solid #d3d3d3"
        , Ha.style "height" "13px"
        , Ha.style "margin" "1px 5px"
        ]
        []


viewIcon : List (S.Attribute (Msg msg)) -> List (Svg (Msg msg)) -> Html (Msg msg)
viewIcon attributes =
    S.svg
        (Sa.width "15"
            :: Sa.height "15"
            :: Sa.viewBox "0 0 24 24"
            :: attributes
        )


viewSubscribeButton : Hoverable -> Bool -> Html (Msg msg)
viewSubscribeButton currentHover isSubscribed =
    let
        title =
            if isSubscribed then
                "Unsubscribe"

            else
                "Subscribe"

        fill =
            if not isSubscribed then
                "#1cabf1"

            else if currentHover == SubscribeButton then
                "black"

            else
                "#7c7c7c"
    in
    viewIcon
        [ Se.onClick ToggleSubscriptions
        , Se.onMouseOver (Hover SubscribeButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.fill fill
            , Sa.d "M14,19H18V5H14M6,19H10V5H6V19Z"
            ]
            []
        , S.title [] [ S.text title ]
        ]


viewDragButton : Hoverable -> Bool -> Html (Msg msg)
viewDragButton currentHover isDragging =
    let
        fill =
            if isDragging then
                "#1cabf1"

            else if DragButton == currentHover then
                "black"

            else
                "#7c7c7c"
    in
    viewIcon
        [ Se.onMouseDown (Drag Start)
        , Se.onMouseOver (Hover DragButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.fill fill
            , Sa.d "M7,19V17H9V19H7M11,19V17H13V19H11M15,19V17H17V19H15M7,15V13H9V15H7M11,15V13H13V15H11M15,15V13H17V15H15M7,11V9H9V11H7M11,11V9H13V11H11M15,11V9H17V11H15M7,7V5H9V7H7M11,7V5H13V7H11M15,7V5H17V7H15Z"
            ]
            []
        , S.title [] [ S.text "Drag to a new position" ]
        ]


viewDismissButton : Hoverable -> Size -> Position -> Html (Msg msg)
viewDismissButton currentHover { height, width } { top, left } =
    let
        fill =
            if DismissButton == currentHover then
                "black"

            else
                "#7c7c7c"
    in
    viewIcon
        [ Se.onClick Dismiss
        , Se.onMouseOver (Hover DismissButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.d "M5,17.59L15.59,7H9V5H19V15H17V8.41L6.41,19L5,17.59Z"
            , Sa.fill fill
            ]
            []
        , S.title [] [ S.text "Dismiss to the upper-right" ]
        ]


viewImportButton : Hoverable -> Maybe Jd.Error -> Html (Msg msg)
viewImportButton currentHover importError =
    let
        fill =
            if ImportButton == currentHover then
                "black"

            else if isJust importError then
                "red"

            else
                "#7c7c7c"

        title =
            case importError of
                Just decodeError ->
                    Jd.errorToString decodeError

                Nothing ->
                    "Import from a file"
    in
    viewIcon
        [ Se.onClick SelectFile
        , Se.onMouseOver (Hover ImportButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.d "M14,2L20,8V20A2,2 0 0,1 18,22H6A2,2 0 0,1 4,20V4A2,2 0 0,1 6,2H14M18,20V9H13V4H6V20H18M12,12L16,16H13.5V19H10.5V16H8L12,12Z"
            , Sa.fill fill
            ]
            []
        , S.title [] [ S.text title ]
        ]


viewExportButton : Hoverable -> Bool -> Html (Msg msg)
viewExportButton currentHover isEnabled =
    let
        attributes =
            if isEnabled then
                [ Se.onClick ExportUpdates
                , Se.onMouseOver (Hover ExportButton)
                , Se.onMouseOut (Hover None)
                ]

            else
                []

        title =
            if isEnabled then
                "Export to a file"

            else
                "You need at least 1 update to export this session"

        fill =
            if ExportButton == currentHover then
                "black"

            else
                "#7c7c7c"
    in
    viewIcon
        attributes
        [ S.path
            [ Sa.d "M14,2L20,8V20A2,2 0 0,1 18,22H6A2,2 0 0,1 4,20V4A2,2 0 0,1 6,2H14M18,20V9H13V4H6V20H18M12,19L8,15H10.5V12H13.5V15H16L12,19Z"
            , Sa.fill fill
            ]
            []
        , S.title [] [ S.text title ]
        ]


viewOverlayButton : Hoverable -> Bool -> Html (Msg msg)
viewOverlayButton currentHover isModelOverlayed =
    let
        title =
            if isModelOverlayed then
                "Hide the model"

            else
                "Inspect the model"

        fill =
            if isModelOverlayed then
                "#1cabf1"

            else if OverlayButton == currentHover then
                "black"

            else
                "#7c7c7c"
    in
    viewIcon
        [ Se.onClick ToggleModelOverlay
        , Se.onMouseOver (Hover OverlayButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.d "M5,3H7V5H5V10A2,2 0 0,1 3,12A2,2 0 0,1 5,14V19H7V21H5C3.93,20.73 3,20.1 3,19V15A2,2 0 0,0 1,13H0V11H1A2,2 0 0,0 3,9V5A2,2 0 0,1 5,3M19,3A2,2 0 0,1 21,5V9A2,2 0 0,0 23,11H24V13H23A2,2 0 0,0 21,15V19A2,2 0 0,1 19,21H17V19H19V14A2,2 0 0,1 21,12A2,2 0 0,1 19,10V5H17V3H19M12,15A1,1 0 0,1 13,16A1,1 0 0,1 12,17A1,1 0 0,1 11,16A1,1 0 0,1 12,15M8,15A1,1 0 0,1 9,16A1,1 0 0,1 8,17A1,1 0 0,1 7,16A1,1 0 0,1 8,15M16,15A1,1 0 0,1 17,16A1,1 0 0,1 16,17A1,1 0 0,1 15,16A1,1 0 0,1 16,15Z"
            , Sa.fill fill
            ]
            []
        , S.title [] [ S.text title ]
        ]


viewLayoutButton : Hoverable -> Layout -> Html (Msg msg)
viewLayoutButton currentHover layout =
    let
        ( title, d ) =
            case layout of
                Expanded ->
                    ( "Collapse to hide controls"
                    , "M19,6.41L17.59,5L12,10.59L6.41,5L5,6.41L10.59,12L5,17.59L6.41,19L12,13.41L17.59,19L19,17.59L13.41,12L19,6.41Z"
                    )

                Collapsed ->
                    ( "Expand to reveal controls"
                    , "M20 8h-2.81c-.45-.78-1.07-1.45-1.82-1.96L17 4.41 15.59 3l-2.17 2.17C12.96 5.06 12.49 5 12 5c-.49 0-.96.06-1.41.17L8.41 3 7 4.41l1.62 1.63C7.88 6.55 7.26 7.22 6.81 8H4v2h2.09c-.05.33-.09.66-.09 1v1H4v2h2v1c0 .34.04.67.09 1H4v2h2.81c1.04 1.79 2.97 3 5.19 3s4.15-1.21 5.19-3H20v-2h-2.09c.05-.33.09-.66.09-1v-1h2v-2h-2v-1c0-.34-.04-.67-.09-1H20V8zm-6 8h-4v-2h4v2zm0-4h-4v-2h4v2z"
                    )

        fill =
            if LayoutButton == currentHover then
                "black"

            else
                "#7c7c7c"
    in
    viewIcon
        [ Se.onClick ToggleLayout
        , Se.onMouseOver (Hover LayoutButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.fill fill
            , Sa.d d
            ]
            []
        , S.title [] [ S.text title ]
        ]


viewOverlay : Size -> String -> Html (Msg msg)
viewOverlay viewportSize text =
    H.div
        [ Ha.style "top" "0"
        , Ha.style "left" "0"
        , Ha.style "color" "black"
        , Ha.style "padding" "5vw"
        , Ha.style "position" "fixed"
        , Ha.style "z-index" "2147483646"
        , Ha.style "background-color" "rgba(255,255,255,.95)"
        , Ha.style "height" (toPx viewportSize.height)
        ]
        [ H.div [] [ H.text text ]
        ]


viewSlider : Int -> Int -> Html (Msg msg)
viewSlider length currentIndex =
    H.input
        [ Ha.style "margin" "0"
        , Ha.style "height" "15px"
        , Ha.style "width" "114px"
        , Ha.type_ "range"
        , Ha.title "Drag to another state"
        , Ha.min (String.fromInt 0)
        , Ha.disabled (length == 1)
        , Ha.max (String.fromInt (length - 1))
        , Ha.value (String.fromInt currentIndex)
        , He.onInput (SelectUpdateAt << Maybe.withDefault 0 << String.toInt)
        , He.onMouseOver (Hover UpdateSlider)
        , He.onMouseOut (Hover None)
        ]
        []


viewUpdate : Hoverable -> Int -> ( Int, String ) -> ( String, Html (Msg msg) )
viewUpdate currentHover currentIndex ( index, json ) =
    let
        ( text, sub, title ) =
            case Jd.decodeString (Jd.keyValuePairs Jd.value) json of
                Ok (( msg, params ) :: []) ->
                    ( msg, Je.encode 0 params, json )

                _ ->
                    ( json, "", "" )

        isSelected =
            index == currentIndex

        ( color, subColor ) =
            if isSelected then
                ( "white", "white" )

            else
                ( "black", "gray" )

        backgroundColor =
            if isSelected then
                "#1cabf1"

            else if UpdateButtonAt index == currentHover then
                "#f1f6fd"

            else if isOdd index then
                "#f5f5f5"

            else
                "white"
    in
    ( String.fromInt index
    , selectable False
        [ Ha.style "height" "18px"
        , Ha.style "padding" "0 5px"
        , Ha.style "line-height" "18px"
        , Ha.style "font-size" "8px"
        , Ha.style "color" color
        , Ha.style "background-color" backgroundColor
        , Ha.title title
        , He.onClick (SelectUpdateAt index)
        , He.onMouseOver (Hover (UpdateButtonAt index))
        , He.onMouseOut (Hover None)
        ]
        [ H.text (fit 10 text)
        , H.span
            [ Ha.style "color" subColor
            , Ha.style "padding-left" "5px"
            , Ha.style "font-size" "7px"
            ]
            [ H.text (fit 12 (String.join "" (String.split "null" sub)))
            ]
        , H.span
            [ Ha.style "float" "right"
            ]
            [ H.text (String.fromInt index)
            ]
        ]
    )


viewNotes : String -> String -> Html (Msg msg)
viewNotes sessionTitle notes =
    H.div []
        [ H.input
            [ Ha.style "display" "block"
            , Ha.style "border" "none"
            , Ha.style "outline" "none"
            , Ha.style "width" "96%"
            , Ha.style "padding" "5px"
            , Ha.style "border-bottom" "1px solid #d3d3d3"
            , Ha.placeholder "elm-dev-tools"
            , Ha.title "The session title"
            , Ha.value sessionTitle
            , He.onInput InputSessionTitle
            ]
            []
        , H.textarea
            [ Ha.style "overflow" "hidden auto"
            , Ha.style "border" "0"
            , Ha.style "outline" "0"
            , Ha.style "display" "block"
            , Ha.style "padding" "5px"
            , Ha.style "height" "149px"
            , Ha.style "width" "170px"
            , Ha.style "resize" "none"
            , Ha.placeholder """You can export this session to a file and send it as a bug-report.

Updates are indexed for your convenience.

Take a moment to describe what you're doing!
"""
            , Ha.title "The session notes"
            , Ha.value notes
            , He.onInput InputNotes
            ]
            []
        ]


viewPage :
    { currentIndex : Int
    , currentHover : Hoverable
    , isSubscribed : Bool
    , layoutSize : Size
    , page : Page
    , updates : List ( Int, String )
    , notes : String
    , sessionTitle : String
    }
    -> Html (Msg msg)
viewPage { currentIndex, currentHover, isSubscribed, layoutSize, page, updates, notes, sessionTitle } =
    let
        body =
            case page of
                Updates ->
                    Hk.node "div" [] (List.map (viewUpdate currentHover currentIndex) updates)

                Notes ->
                    viewNotes sessionTitle notes
    in
    H.div
        [ Ha.style "border-bottom" "1px solid #d3d3d3"
        , Ha.style "overflow" "hidden"
        , Ha.style "height" "180px"
        ]
        [ body ]


viewControls : List (Html (Msg msg)) -> Html (Msg msg)
viewControls =
    H.div
        [ Ha.style "height" "20px"
        , Ha.style "display" "flex"
        , Ha.style "padding" "0 5px"
        , Ha.style "align-items" "center"
        , Ha.style "background-color" "#f3f3f3"
        , Ha.style "border-bottom" "1px solid #d3d3d3"
        ]


viewNavigationPage : Int -> Page -> Page -> Hoverable -> Html (Msg msg)
viewNavigationPage index page currentPage currentHover =
    let
        this =
            NavigationButtonFor page

        ( color, backgroundColor ) =
            if page == currentPage then
                ( "black", "rgba(0,0,0,0)" )

            else if this == currentHover then
                ( "black", "rgba(0,0,0,.03)" )

            else
                ( "#555555", "rgba(0,0,0,0)" )

        ( label, title ) =
            case page of
                Updates ->
                    ( "Updates", "List of messages" )

                Notes ->
                    ( "Notes", "Notes on the development session" )
    in
    selectable False
        [ Ha.style "font-size" "9px"
        , Ha.style "height" "20px"
        , Ha.style "line-height" "20px"
        , Ha.style "padding" "0 6.5px"
        , Ha.style "background-color" backgroundColor
        , Ha.style "color" color
        , Ha.title title
        , He.onMouseOver (Hover this)
        , He.onMouseOut (Hover None)
        , He.onClick (SelectPage page)
        ]
        [ H.text label
        ]


viewNavigationUnderline : Page -> Html msg
viewNavigationUnderline page =
    let
        ( width, transform ) =
            case page of
                Updates ->
                    ( 50, "translate(0,0)" )

                Notes ->
                    ( 39, "translate(50px,0)" )
    in
    H.div
        [ Ha.style "position" "absolute"
        , Ha.style "top" "19px"
        , Ha.style "left" "76px"
        , Ha.style "transition" "transform 140ms ease-out, width 140ms ease-out"
        , Ha.style "border-bottom" "2px solid #1cabf1"
        , Ha.style "width" (toPx width)
        , Ha.style "transform" transform
        ]
        []
