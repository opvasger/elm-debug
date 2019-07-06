module Icon exposing
    ( Icon(..)
    , viewDownload
    , viewDrag
    , viewJson
    , viewPlay
    , viewReplay
    , viewUpdate
    , viewUpload
    )

import Help
import History.DecodeStrategy as DecodeStrategy exposing (DecodeStrategy)
import Html exposing (Html)
import Svg exposing (Svg, path, svg, text, title)
import Svg.Attributes exposing (d, fill, style, viewBox)
import Svg.Events exposing (onClick, onMouseOut, onMouseOver)


type Icon
    = Replay
    | Play
    | Json
    | Download
    | Upload
    | Update


type alias Config record msg =
    { record
        | onClick : msg
        , onFocus : Maybe Icon -> msg
        , title : String
        , focus : Maybe Icon
    }


toIcon : Config record msg -> Icon -> List (Svg msg) -> Html msg
toIcon config icon =
    (::) (title [] [ text config.title ])
        >> svg
            [ style "width:20px;height:20px;cursor:pointer;-ms-user-select:none;-webkit-user-select:none;-moz-user-select:none;user-select:none;"
            , viewBox "0 0 24 24"
            , onClick config.onClick
            , onMouseOver (config.onFocus (Just icon))
            , onMouseOut (config.onFocus Nothing)
            ]


viewDrag : Html msg
viewDrag =
    svg
        [ style "width:20px;height:20px;-ms-user-select:none;-webkit-user-select:none;-moz-user-select:none;user-select:none;"
        , viewBox "0 0 24 24"
        ]
        [ path
            [ fill Help.mutedGray
            , d "M7,19V17H9V19H7M11,19V17H13V19H11M15,19V17H17V19H15M7,15V13H9V15H7M11,15V13H13V15H11M15,15V13H17V15H15M7,11V9H9V11H7M11,11V9H13V11H11M15,11V9H17V11H15M7,7V5H9V7H7M11,7V5H13V7H11M15,7V5H17V7H15Z"
            ]
            []
        ]


viewReplay : Config record msg -> Html msg
viewReplay config =
    toIcon config
        Replay
        [ path
            [ if Help.isJust Replay config.focus then
                fill Help.focusBlack

              else
                fill Help.mutedGray
            , d "M12,5V1L7,6L12,11V7A6,6 0 0,1 18,13A6,6 0 0,1 12,19A6,6 0 0,1 6,13H4A8,8 0 0,0 12,21A8,8 0 0,0 20,13A8,8 0 0,0 12,5Z"
            ]
            []
        ]


viewPlay : Config { isPartial : Bool, isPlay : Bool } msg -> Html msg
viewPlay config =
    toIcon config
        Play
        [ path
            [ if config.isPlay then
                fill Help.activeBlue

              else if Help.isJust Play config.focus then
                fill Help.focusBlack

              else
                fill Help.mutedGray
            , if config.isPartial then
                d "M13,2.05V4.05C17.39,4.59 20.5,8.58 19.96,12.97C19.5,16.61 16.64,19.5 13,19.93V21.93C18.5,21.38 22.5,16.5 21.95,11C21.5,6.25 17.73,2.5 13,2.03V2.05M5.67,19.74C7.18,21 9.04,21.79 11,22V20C9.58,19.82 8.23,19.25 7.1,18.37L5.67,19.74M7.1,5.74C8.22,4.84 9.57,4.26 11,4.06V2.06C9.05,2.25 7.19,3 5.67,4.26L7.1,5.74M5.69,7.1L4.26,5.67C3,7.19 2.25,9.04 2.05,11H4.05C4.24,9.58 4.8,8.23 5.69,7.1M4.06,13H2.06C2.26,14.96 3.03,16.81 4.27,18.33L5.69,16.9C4.81,15.77 4.24,14.42 4.06,13M10,16.5L16,12L10,7.5V16.5Z"

              else
                d "M12,20C7.59,20 4,16.41 4,12C4,7.59 7.59,4 12,4C16.41,4 20,7.59 20,12C20,16.41 16.41,20 12,20M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2M10,16.5L16,12L10,7.5V16.5Z"
            ]
            []
        ]


viewJson : Config { isModelVisible : Bool } msg -> Html msg
viewJson config =
    toIcon config
        Json
        [ path
            [ if config.isModelVisible then
                fill Help.activeBlue

              else if Help.isJust Json config.focus then
                fill Help.focusBlack

              else
                fill Help.mutedGray
            , d "M5,3H7V5H5V10A2,2 0 0,1 3,12A2,2 0 0,1 5,14V19H7V21H5C3.93,20.73 3,20.1 3,19V15A2,2 0 0,0 1,13H0V11H1A2,2 0 0,0 3,9V5A2,2 0 0,1 5,3M19,3A2,2 0 0,1 21,5V9A2,2 0 0,0 23,11H24V13H23A2,2 0 0,0 21,15V19A2,2 0 0,1 19,21H17V19H19V14A2,2 0 0,1 21,12A2,2 0 0,1 19,10V5H17V3H19M12,15A1,1 0 0,1 13,16A1,1 0 0,1 12,17A1,1 0 0,1 11,16A1,1 0 0,1 12,15M8,15A1,1 0 0,1 9,16A1,1 0 0,1 8,17A1,1 0 0,1 7,16A1,1 0 0,1 8,15M16,15A1,1 0 0,1 17,16A1,1 0 0,1 16,17A1,1 0 0,1 15,16A1,1 0 0,1 16,15Z"
            ]
            []
        ]


viewDownload : Config record msg -> Html msg
viewDownload config =
    toIcon config
        Download
        [ path
            [ if Help.isJust Download config.focus then
                fill Help.focusBlack

              else
                fill Help.mutedGray
            , d "M5,20H19V18H5M19,9H15V3H9V9H5L12,16L19,9Z"
            ]
            []
        ]


viewUpload : Config { isFailed : Bool } msg -> Html msg
viewUpload config =
    toIcon config
        Upload
        [ path
            [ if config.isFailed then
                fill Help.errorRed

              else if Help.isJust Upload config.focus then
                fill Help.focusBlack

              else
                fill Help.mutedGray
            , d "M9,16V10H5L12,3L19,10H15V16H9M5,20V18H19V20H5Z"
            ]
            []
        ]


viewUpdate : Config { strategy : DecodeStrategy } msg -> Html msg
viewUpdate config =
    let
        ( color, title ) =
            case config.strategy of
                DecodeStrategy.NoErrors ->
                    ( if Help.isJust Update config.focus then
                        Help.focusBlack

                      else
                        Help.mutedGray
                    , "This session reloads state if no messages were changed or removed."
                    )

                DecodeStrategy.UntilError ->
                    ( Help.activeBlue
                    , "This session reloads state until the first unknown message."
                    )

                DecodeStrategy.SkipErrors ->
                    ( Help.errorRed
                    , "This session reloads state and skips unknown messages."
                    )
    in
    toIcon { config | title = title }
        Update
        [ path
            [ fill color
            , d "M13,2.03V2.05L13,4.05C17.39,4.59 20.5,8.58 19.96,12.97C19.5,16.61 16.64,19.5 13,19.93V21.93C18.5,21.38 22.5,16.5 21.95,11C21.5,6.25 17.73,2.5 13,2.03M11,2.06C9.05,2.25 7.19,3 5.67,4.26L7.1,5.74C8.22,4.84 9.57,4.26 11,4.06V2.06M4.26,5.67C3,7.19 2.25,9.04 2.05,11H4.05C4.24,9.58 4.8,8.23 5.69,7.1L4.26,5.67M2.06,13C2.26,14.96 3.03,16.81 4.27,18.33L5.69,16.9C4.81,15.77 4.24,14.42 4.06,13H2.06M7.1,18.37L5.67,19.74C7.18,21 9.04,21.79 11,22V20C9.58,19.82 8.23,19.25 7.1,18.37M12.5,7V12.25L17,14.92L16.25,16.15L11,13V7H12.5Z"
            ]
            []
        ]
