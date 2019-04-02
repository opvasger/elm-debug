module DevTools.Browser.Elements exposing
    ( HoverTarget
    , noTarget
    , viewDebugger
    , viewModelOverlay
    )

import DevTools.Browser.Icons as Icons
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html exposing (Html)
import Html.Events
import Json.Decode as Jd


type HoverTarget
    = ToggleReplayButton
    | ToggleOverlayButton
    | ImportSessionButton
    | ExportSessionButton
    | NoTarget


noTarget : HoverTarget
noTarget =
    NoTarget


borderGray : Color
borderGray =
    rgb255 211 211 211


backgroundGray : Color
backgroundGray =
    rgb255 243 243 243


white : Color
white =
    rgb255 255 255 255


viewDivider : Element msg
viewDivider =
    el
        [ paddingXY 3 0
        ]
        (el
            [ width (px 1)
            , height (px 16)
            , Background.color borderGray
            ]
            none
        )


viewControls : (Int -> Int -> msg) -> List (Element msg) -> Element msg
viewControls onMouseDown =
    row
        [ height (px 27)
        , Border.width 1
        , Border.color borderGray
        , Background.color backgroundGray
        , paddingXY 5 0
        , spacing 1
        , width fill
        , onEvent "mousedown" (mousePositionDecoder onMouseDown)
        ]


mousePositionDecoder : (Int -> Int -> msg) -> Jd.Decoder msg
mousePositionDecoder toMsg =
    Jd.map2 toMsg
        (Jd.field "clientX" Jd.int)
        (Jd.field "clientY" Jd.int)


onEvent : String -> Jd.Decoder msg -> Element.Attribute msg
onEvent eventName =
    htmlAttribute << Html.Events.on eventName


viewBody :
    { height : Int
    , body : Element msg
    }
    -> Element msg
viewBody config =
    el
        [ height (px config.height)
        , width fill
        , Border.widthXY 1 0
        , Border.color borderGray
        , Background.color white
        ]
        config.body


viewSlider :
    { value : Int
    , maxValue : Int
    , onChange : Int -> msg
    }
    -> Element msg
viewSlider config =
    Input.slider
        [ width fill
        , height (px 18)
        , behindContent (viewSliderTrack config.maxValue config.value)
        ]
        { onChange = config.onChange << round
        , label = Input.labelHidden ""
        , min = 0
        , max = toFloat config.maxValue
        , value = toFloat config.value
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
    row
        [ width fill
        , height (px 6)
        , centerY
        , Border.rounded 100
        , Border.color (rgb255 141 141 141)
        , Border.width 1
        ]
        [ el
            [ width (fillPortion leftPortion)
            , height fill
            , Background.color (rgb255 28 171 241)
            ]
            none
        , el
            [ width (fillPortion rightPortion)
            , height fill
            , Background.gradient
                { angle = 0
                , steps =
                    [ rgb255 194 194 194
                    , rgb255 163 163 163
                    ]
                }
            ]
            none
        ]


viewIconButton :
    { isActive : Bool
    , target : HoverTarget
    , hoverTarget : HoverTarget
    , onChange : msg
    , onHover : HoverTarget -> msg
    , icon : Icons.Style -> Element msg
    , error : Maybe String
    }
    -> Element msg
viewIconButton config =
    let
        toIconStyle isActive =
            if isActive then
                Icons.Active

            else
                case config.error of
                    Just error ->
                        Icons.Error error

                    Nothing ->
                        if config.target == config.hoverTarget then
                            Icons.Hover

                        else
                            Icons.Normal
    in
    Input.checkbox
        [ width shrink
        , Events.onMouseEnter (config.onHover config.target)
        , Events.onMouseLeave (config.onHover NoTarget)
        ]
        { onChange = always config.onChange
        , checked = config.isActive
        , label = Input.labelHidden ""
        , icon = config.icon << toIconStyle
        }


viewModelOverlay :
    { printModel : model -> String
    , model : model
    , isEnabled : Bool
    }
    -> Element msg
viewModelOverlay config =
    if not config.isEnabled then
        none

    else
        text (config.printModel config.model)


viewDebugger :
    { width : Int
    , bodyHeight : Int
    , leftPosition : Int
    , topPosition : Int
    , hoverTarget : HoverTarget
    , hoverTargetMsg : HoverTarget -> msg
    , isModelOverlayed : Bool
    , toggleOverlayMsg : msg
    , isReplaying : Bool
    , toggleReplayMsg : msg
    , currentModelIndex : Int
    , modelIndexLength : Int
    , changeModelIndexMsg : Int -> msg
    , selectModelMsg : msg
    , loadModelError : Maybe Jd.Error
    , saveModelMsg : msg
    , dragStartMsg : Int -> Int -> msg
    , doNothingMsg : msg
    }
    -> Element msg
viewDebugger config =
    column
        [ width (px config.width)
        , moveRight (toFloat config.leftPosition)
        , moveDown (toFloat config.topPosition)
        ]
        [ viewControls config.dragStartMsg
            [ viewIconButton
                { isActive = config.isModelOverlayed
                , target = ToggleOverlayButton
                , hoverTarget = config.hoverTarget
                , onHover = config.hoverTargetMsg
                , onChange = config.toggleOverlayMsg
                , icon = Icons.viewModelIcon
                , error = Nothing
                }
            , viewIconButton
                { isActive = False
                , target = ImportSessionButton
                , hoverTarget = config.hoverTarget
                , onHover = config.hoverTargetMsg
                , onChange = config.selectModelMsg
                , icon = Icons.viewImportIcon
                , error = Maybe.map Jd.errorToString config.loadModelError
                }
            , viewIconButton
                { isActive = False
                , target = ExportSessionButton
                , hoverTarget = config.hoverTarget
                , onHover = config.hoverTargetMsg
                , onChange = config.saveModelMsg
                , icon = Icons.viewExportIcon
                , error = Nothing
                }
            , viewDivider
            ]
        , viewBody
            { height = config.bodyHeight
            , body = none
            }
        , viewControls (\_ _ -> config.doNothingMsg)
            [ viewSlider
                { value = config.currentModelIndex
                , maxValue = config.modelIndexLength
                , onChange = config.changeModelIndexMsg
                }
            , viewDivider
            , viewIconButton
                { isActive = config.isReplaying
                , target = ToggleReplayButton
                , hoverTarget = config.hoverTarget
                , onHover = config.hoverTargetMsg
                , onChange = config.toggleReplayMsg
                , icon = Icons.viewPauseIcon
                , error = Nothing
                }
            ]
        ]
