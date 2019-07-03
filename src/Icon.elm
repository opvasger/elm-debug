module Icon exposing
    ( Icon(..)
    , viewDownload
    , viewJson
    , viewPlay
    , viewReplay
    , viewUpload
    )

import Help
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
            [ style "width:20px;height:20px;cursor:pointer;"
            , viewBox "0 0 24 24"
            , onClick config.onClick
            , onMouseOver (config.onFocus (Just icon))
            , onMouseOut (config.onFocus Nothing)
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


viewPlay : Config { isPlay : Bool } msg -> Html msg
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
            , d "M8,5.14V19.14L19,12.14L8,5.14Z"
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
