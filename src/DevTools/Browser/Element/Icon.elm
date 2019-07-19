module DevTools.Browser.Element.Icon exposing
    ( Icon
    , viewCheckbox
    , viewCollapse
    , viewDismiss
    , viewDownload
    , viewExpand
    , viewMessages
    , viewModel
    , viewPause
    , viewPlay
    , viewReport
    , viewRestart
    , viewSettings
    , viewUpload
    )

import Html exposing (Html)
import Svg exposing (path, svg, text, title)
import Svg.Attributes exposing (d, fill, style, viewBox)
import Svg.Events exposing (onClick, onMouseOut, onMouseOver)


type Icon
    = Collapse
    | Expand
    | ViewModel
    | Upload
    | Download
    | Dismiss
    | Restart
    | Play
    | Pause
    | Checkbox Int
    | ViewReport
    | ViewMessages
    | ViewSettings


viewCollapse :
    { onFocus : Maybe Icon -> msg
    , onClick : msg
    , title : String
    , focus : Maybe Icon
    }
    -> Html msg
viewCollapse config =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , onMouseOver (config.onFocus (Just Collapse))
        , onMouseOut (config.onFocus Nothing)
        , onClick config.onClick
        ]
        [ title [] [ text config.title ]
        , path
            [ if config.focus == Just Collapse then
                fill "black"

              else
                fill "#6e6e6e"
            , d "M19,6.41L17.59,5L12,10.59L6.41,5L5,6.41L10.59,12L5,17.59L6.41,19L12,13.41L17.59,19L19,17.59L13.41,12L19,6.41Z"
            ]
            []
        ]


viewExpand :
    { onFocus : Maybe Icon -> msg
    , onClick : msg
    , title : String
    , focus : Maybe Icon
    }
    -> Html msg
viewExpand config =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , onMouseOver (config.onFocus (Just Expand))
        , onMouseOut (config.onFocus Nothing)
        , onClick config.onClick
        ]
        [ title [] [ text config.title ]
        , path
            [ if config.focus == Just Expand then
                fill "black"

              else
                fill "#6e6e6e"
            , d "M14,12H10V10H14M14,16H10V14H14M20,8H17.19C16.74,7.22 16.12,6.55 15.37,6.04L17,4.41L15.59,3L13.42,5.17C12.96,5.06 12.5,5 12,5C11.5,5 11.04,5.06 10.59,5.17L8.41,3L7,4.41L8.62,6.04C7.88,6.55 7.26,7.22 6.81,8H4V10H6.09C6.04,10.33 6,10.66 6,11V12H4V14H6V15C6,15.34 6.04,15.67 6.09,16H4V18H6.81C7.85,19.79 9.78,21 12,21C14.22,21 16.15,19.79 17.19,18H20V16H17.91C17.96,15.67 18,15.34 18,15V14H20V12H18V11C18,10.66 17.96,10.33 17.91,10H20V8Z"
            ]
            []
        ]


viewModel :
    { isEnabled : Bool
    , onFocus : Maybe Icon -> msg
    , onClick : msg
    , title : String
    , focus : Maybe Icon
    }
    -> Html msg
viewModel config =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "-2 0 26 26"
        , onMouseOver (config.onFocus (Just ViewModel))
        , onMouseOut (config.onFocus Nothing)
        , onClick config.onClick
        ]
        [ title [] [ text config.title ]
        , path
            [ if config.isEnabled then
                fill "#60b5cc"

              else if config.focus == Just ViewModel then
                fill "black"

              else
                fill "#6e6e6e"
            , d "M5,3H7V5H5V10A2,2 0 0,1 3,12A2,2 0 0,1 5,14V19H7V21H5C3.93,20.73 3,20.1 3,19V15A2,2 0 0,0 1,13H0V11H1A2,2 0 0,0 3,9V5A2,2 0 0,1 5,3M19,3A2,2 0 0,1 21,5V9A2,2 0 0,0 23,11H24V13H23A2,2 0 0,0 21,15V19A2,2 0 0,1 19,21H17V19H19V14A2,2 0 0,1 21,12A2,2 0 0,1 19,10V5H17V3H19M12,15A1,1 0 0,1 13,16A1,1 0 0,1 12,17A1,1 0 0,1 11,16A1,1 0 0,1 12,15M8,15A1,1 0 0,1 9,16A1,1 0 0,1 8,17A1,1 0 0,1 7,16A1,1 0 0,1 8,15M16,15A1,1 0 0,1 17,16A1,1 0 0,1 16,17A1,1 0 0,1 15,16A1,1 0 0,1 16,15Z"
            ]
            []
        ]


viewUpload :
    { onFocus : Maybe Icon -> msg
    , onClick : msg
    , title : String
    , focus : Maybe Icon
    , hasFailed : Bool
    }
    -> Html msg
viewUpload config =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , onMouseOver (config.onFocus (Just Upload))
        , onMouseOut (config.onFocus Nothing)
        , onClick config.onClick
        ]
        [ title [] [ text config.title ]
        , path
            [ if config.hasFailed then
                fill "red"

              else if config.focus == Just Upload then
                fill "black"

              else
                fill "#6e6e6e"
            , d "M9,16V10H5L12,3L19,10H15V16H9M5,20V18H19V20H5Z"
            ]
            []
        ]


viewDownload :
    { onFocus : Maybe Icon -> msg
    , onClick : msg
    , title : String
    , focus : Maybe Icon
    }
    -> Html msg
viewDownload config =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , onMouseOver (config.onFocus (Just Download))
        , onMouseOut (config.onFocus Nothing)
        , onClick config.onClick
        ]
        [ title [] [ text config.title ]
        , path
            [ if config.focus == Just Download then
                fill "black"

              else
                fill "#6e6e6e"
            , d "M5,20H19V18H5M19,9H15V3H9V9H5L12,16L19,9Z"
            ]
            []
        ]


viewDismiss :
    { onFocus : Maybe Icon -> msg
    , onClick : msg
    , title : String
    , focus : Maybe Icon
    }
    -> Html msg
viewDismiss config =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , onMouseOver (config.onFocus (Just Dismiss))
        , onMouseOut (config.onFocus Nothing)
        , onClick config.onClick
        ]
        [ title [] [ text config.title ]
        , path
            [ if config.focus == Just Dismiss then
                fill "black"

              else
                fill "#6e6e6e"
            , d "M5,6.41L6.41,5L17,15.59V9H19V19H9V17H15.59L5,6.41Z"
            ]
            []
        ]


viewRestart :
    { onFocus : Maybe Icon -> msg
    , onClick : msg
    , title : String
    , focus : Maybe Icon
    }
    -> Html msg
viewRestart config =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , onMouseOver (config.onFocus (Just Restart))
        , onMouseOut (config.onFocus Nothing)
        , onClick config.onClick
        ]
        [ title [] [ text config.title ]
        , path
            [ if config.focus == Just Restart then
                fill "black"

              else
                fill "#6e6e6e"
            , d "M12,4C14.1,4 16.1,4.8 17.6,6.3C20.7,9.4 20.7,14.5 17.6,17.6C15.8,19.5 13.3,20.2 10.9,19.9L11.4,17.9C13.1,18.1 14.9,17.5 16.2,16.2C18.5,13.9 18.5,10.1 16.2,7.7C15.1,6.6 13.5,6 12,6V10.6L7,5.6L12,0.6V4M6.3,17.6C3.7,15 3.3,11 5.1,7.9L6.6,9.4C5.5,11.6 5.9,14.4 7.8,16.2C8.3,16.7 8.9,17.1 9.6,17.4L9,19.4C8,19 7.1,18.4 6.3,17.6Z"
            ]
            []
        ]


viewPlay :
    { onFocus : Maybe Icon -> msg
    , onClick : msg
    , title : String
    , focus : Maybe Icon
    }
    -> Html msg
viewPlay config =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , onMouseOver (config.onFocus (Just Play))
        , onMouseOut (config.onFocus Nothing)
        , onClick config.onClick
        ]
        [ title [] [ text config.title ]
        , path
            [ if config.focus == Just Play then
                fill "black"

              else
                fill "#6e6e6e"
            , d "M8,5.14V19.14L19,12.14L8,5.14Z"
            ]
            []
        ]


viewPause :
    { onFocus : Maybe Icon -> msg
    , onClick : msg
    , title : String
    , focus : Maybe Icon
    }
    -> Html msg
viewPause config =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , onMouseOver (config.onFocus (Just Pause))
        , onMouseOut (config.onFocus Nothing)
        , onClick config.onClick
        ]
        [ title [] [ text config.title ]
        , path
            [ if config.focus == Just Pause then
                fill "black"

              else
                fill "#6e6e6e"
            , d "M14,19H18V5H14M6,19H10V5H6V19Z"
            ]
            []
        ]


viewSettings :
    { onFocus : Maybe Icon -> msg
    , onClick : msg
    , title : String
    , focus : Maybe Icon
    , isActive : Bool
    }
    -> Html msg
viewSettings config =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , onMouseOver (config.onFocus (Just ViewSettings))
        , onMouseOut (config.onFocus Nothing)
        , onClick config.onClick
        ]
        [ title [] [ text config.title ]
        , path
            [ if config.isActive then
                fill "#60b5cc"

              else if config.focus == Just ViewSettings then
                fill "black"

              else
                fill "#6e6e6e"
            , d "M12,15.5A3.5,3.5 0 0,1 8.5,12A3.5,3.5 0 0,1 12,8.5A3.5,3.5 0 0,1 15.5,12A3.5,3.5 0 0,1 12,15.5M19.43,12.97C19.47,12.65 19.5,12.33 19.5,12C19.5,11.67 19.47,11.34 19.43,11L21.54,9.37C21.73,9.22 21.78,8.95 21.66,8.73L19.66,5.27C19.54,5.05 19.27,4.96 19.05,5.05L16.56,6.05C16.04,5.66 15.5,5.32 14.87,5.07L14.5,2.42C14.46,2.18 14.25,2 14,2H10C9.75,2 9.54,2.18 9.5,2.42L9.13,5.07C8.5,5.32 7.96,5.66 7.44,6.05L4.95,5.05C4.73,4.96 4.46,5.05 4.34,5.27L2.34,8.73C2.21,8.95 2.27,9.22 2.46,9.37L4.57,11C4.53,11.34 4.5,11.67 4.5,12C4.5,12.33 4.53,12.65 4.57,12.97L2.46,14.63C2.27,14.78 2.21,15.05 2.34,15.27L4.34,18.73C4.46,18.95 4.73,19.03 4.95,18.95L7.44,17.94C7.96,18.34 8.5,18.68 9.13,18.93L9.5,21.58C9.54,21.82 9.75,22 10,22H14C14.25,22 14.46,21.82 14.5,21.58L14.87,18.93C15.5,18.67 16.04,18.34 16.56,17.94L19.05,18.95C19.27,19.03 19.54,18.95 19.66,18.73L21.66,15.27C21.78,15.05 21.73,14.78 21.54,14.63L19.43,12.97Z"
            ]
            []
        ]


viewReport :
    { onFocus : Maybe Icon -> msg
    , onClick : msg
    , title : String
    , focus : Maybe Icon
    , isActive : Bool
    }
    -> Html msg
viewReport config =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , onMouseOver (config.onFocus (Just ViewReport))
        , onMouseOut (config.onFocus Nothing)
        , onClick config.onClick
        ]
        [ title [] [ text config.title ]
        , path
            [ if config.isActive then
                fill "#60b5cc"

              else if config.focus == Just ViewReport then
                fill "black"

              else
                fill "#6e6e6e"
            , d "M20,2H4A2,2 0 0,0 2,4V22L6,18H20A2,2 0 0,0 22,16V4A2,2 0 0,0 20,2M6,9H18V11H6M14,14H6V12H14M18,8H6V6H18"
            ]
            []
        ]


viewMessages :
    { onFocus : Maybe Icon -> msg
    , onClick : msg
    , title : String
    , focus : Maybe Icon
    , isActive : Bool
    }
    -> Html msg
viewMessages config =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , onMouseOver (config.onFocus (Just ViewMessages))
        , onMouseOut (config.onFocus Nothing)
        , onClick config.onClick
        ]
        [ title [] [ text config.title ]
        , path
            [ if config.isActive then
                fill "#60b5cc"

              else if config.focus == Just ViewMessages then
                fill "black"

              else
                fill "#6e6e6e"
            , d "M3,13V11H17V13H3M3,19V17H17V19H3M3,7V5H17V7H3M20,8V5H19V4H21V8H20M19,17V16H22V20H19V19H21V18.5H20V17.5H21V17H19M21.25,10C21.67,10 22,10.34 22,10.75C22,10.95 21.92,11.14 21.79,11.27L20.12,13H22V14H19V13.08L21,11H19V10H21.25Z"
            ]
            []
        ]


viewCheckbox :
    { onFocus : Maybe Icon -> msg
    , onClick : msg
    , title : String
    , focus : Maybe Icon
    , isActive : Bool
    , key : Int
    }
    -> Html msg
viewCheckbox config =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , onMouseOver (config.onFocus (Just (Checkbox config.key)))
        , onMouseOut (config.onFocus Nothing)
        , onClick config.onClick
        ]
        [ title [] [ text config.title ]
        , path
            [ if config.isActive then
                fill "#60b5cc"

              else if config.focus == Just (Checkbox config.key) then
                fill "black"

              else
                fill "#6e6e6e"
            , if config.isActive then
                d "M10,17L5,12L6.41,10.58L10,14.17L17.59,6.58L19,8M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2Z"

              else
                d "M12,20A8,8 0 0,1 4,12A8,8 0 0,1 12,4A8,8 0 0,1 20,12A8,8 0 0,1 12,20M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2Z"
            ]
            []
        ]
