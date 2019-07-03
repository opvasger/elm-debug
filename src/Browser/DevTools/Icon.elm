module Browser.DevTools.Icon exposing
    ( loadSession
    , pauseSession
    , restartSession
    , resumeSession
    , saveSession
    , toggleModel
    )

import Html exposing (Html)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, style, title, viewBox)
import Svg.Events exposing (onClick)


restartSession : msg -> Html msg
restartSession msg =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , title "Restart session"
        , onClick msg
        ]
        [ path
            [ fill "#6e6e6e"
            , d "M12,5V1L7,6L12,11V7A6,6 0 0,1 18,13A6,6 0 0,1 12,19A6,6 0 0,1 6,13H4A8,8 0 0,0 12,21A8,8 0 0,0 20,13A8,8 0 0,0 12,5Z"
            ]
            []
        ]


pauseSession : msg -> Html msg
pauseSession msg =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , title "Pause session"
        , onClick msg
        ]
        [ path
            [ fill "#6e6e6e"
            , d "M14,19H18V5H14M6,19H10V5H6V19Z"
            ]
            []
        ]


resumeSession : msg -> Html msg
resumeSession msg =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , title "Resume session"
        , onClick msg
        ]
        [ path
            [ fill "#6e6e6e"
            , d "M8,5.14V19.14L19,12.14L8,5.14Z"
            ]
            []
        ]


toggleModel : msg -> Html msg
toggleModel msg =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , title "View model"
        , onClick msg
        ]
        [ path
            [ fill "#6e6e6e"
            , d "M5,3H7V5H5V10A2,2 0 0,1 3,12A2,2 0 0,1 5,14V19H7V21H5C3.93,20.73 3,20.1 3,19V15A2,2 0 0,0 1,13H0V11H1A2,2 0 0,0 3,9V5A2,2 0 0,1 5,3M19,3A2,2 0 0,1 21,5V9A2,2 0 0,0 23,11H24V13H23A2,2 0 0,0 21,15V19A2,2 0 0,1 19,21H17V19H19V14A2,2 0 0,1 21,12A2,2 0 0,1 19,10V5H17V3H19M12,15A1,1 0 0,1 13,16A1,1 0 0,1 12,17A1,1 0 0,1 11,16A1,1 0 0,1 12,15M8,15A1,1 0 0,1 9,16A1,1 0 0,1 8,17A1,1 0 0,1 7,16A1,1 0 0,1 8,15M16,15A1,1 0 0,1 17,16A1,1 0 0,1 16,17A1,1 0 0,1 15,16A1,1 0 0,1 16,15Z"
            ]
            []
        ]


saveSession : msg -> Html msg
saveSession msg =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , title "Save session"
        , onClick msg
        ]
        [ path
            [ fill "#6e6e6e"
            , d "M5,20H19V18H5M19,9H15V3H9V9H5L12,16L19,9Z"
            ]
            []
        ]


loadSession : msg -> Html msg
loadSession msg =
    svg
        [ style "width:20px;height:20px;cursor:pointer;"
        , viewBox "0 0 24 24"
        , title "Load session"
        , onClick msg
        ]
        [ path
            [ fill "#6e6e6e"
            , d "M9,16V10H5L12,3L19,10H15V16H9M5,20V18H19V20H5Z"
            ]
            []
        ]
