module DevTools.Elements exposing
    ( HoverTarget
    , noTarget
    , viewBody
    , viewControls
    , viewDebugger
    , viewDivider
    , viewIconButton
    , viewModelOverlay
    , viewSlider
    )

import DevTools.Icons as Icons
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html exposing (Html)
import Json.Decode


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
        [ width (px 1)
        , height (px 16)
        , Background.color borderGray
        ]
        none


viewControls : List (Element msg) -> Element msg
viewControls =
    row
        [ height (px 27)
        , Border.width 1
        , Border.color borderGray
        , Background.color backgroundGray
        , width fill
        ]


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


viewSliderTrack : Element msg
viewSliderTrack =
    el
        [ width fill
        , height (px 2)
        , centerY
        , Background.color borderGray
        ]
        none


viewSlider :
    { value : Int
    , maxValue : Int
    , onChange : Int -> msg
    }
    -> Element msg
viewSlider config =
    Input.slider
        [ width fill
        , behindContent viewSliderTrack
        ]
        { onChange = config.onChange << round
        , label = Input.labelHidden ""
        , min = 0
        , max = toFloat config.maxValue
        , value = toFloat config.value
        , thumb = Input.defaultThumb
        , step = Just 1
        }


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
    , loadModelError : Maybe Json.Decode.Error
    , saveModelMsg : msg
    }
    -> Element msg
viewDebugger config =
    column
        [ width (px config.width)
        , moveRight (toFloat config.leftPosition)
        , moveDown (toFloat config.topPosition)
        ]
        [ viewControls
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
                { isActive = config.isReplaying
                , target = ToggleReplayButton
                , hoverTarget = config.hoverTarget
                , onHover = config.hoverTargetMsg
                , onChange = config.toggleReplayMsg
                , icon = Icons.viewPauseIcon
                , error = Nothing
                }
            , viewIconButton
                { isActive = False
                , target = ImportSessionButton
                , hoverTarget = config.hoverTarget
                , onHover = config.hoverTargetMsg
                , onChange = config.selectModelMsg
                , icon = Icons.viewImportIcon
                , error = Maybe.map Json.Decode.errorToString config.loadModelError
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
        , viewControls
            [ viewSlider
                { value = config.currentModelIndex
                , maxValue = config.modelIndexLength
                , onChange = config.changeModelIndexMsg
                }
            , viewDivider
            ]
        ]
