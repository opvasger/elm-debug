module DevTools.Browser.Icons exposing
    ( Style(..)
    , viewExportIcon
    , viewImportIcon
    , viewModelIcon
    , viewPlayIcon
    , viewResetIcon
    )

import Element exposing (Element)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


type alias IconConfig =
    { viewBox : ViewBox
    , path : String
    , style : Style
    , title : String
    }


type ViewBox
    = ViewBox Int Int Int Int


viewIcon : IconConfig -> Element msg
viewIcon config =
    Element.html <|
        svg
            [ width "18"
            , viewBox (viewBoxToString config.viewBox)
            ]
            [ Svg.path [ d config.path, fill (styleToHexColor config.style) ] []
            , Svg.title [] [ text config.title ]
            ]


viewBoxToString : ViewBox -> String
viewBoxToString (ViewBox a b c d) =
    String.fromInt a
        ++ " "
        ++ String.fromInt b
        ++ " "
        ++ String.fromInt c
        ++ " "
        ++ String.fromInt d



-- Style


type Style
    = Normal
    | Hover
    | Active
    | Error String


styleToHexColor : Style -> String
styleToHexColor style =
    case style of
        Normal ->
            "#7c7c7c"

        Hover ->
            "#000000"

        Active ->
            "#1cabf1"

        Error _ ->
            "#ff0000"



-- Icons


viewModelIcon : Style -> Element msg
viewModelIcon style =
    viewIcon
        { viewBox = ViewBox 0 0 24 24
        , style = style
        , path = "M5,3H7V5H5V10A2,2 0 0,1 3,12A2,2 0 0,1 5,14V19H7V21H5C3.93,20.73 3,20.1 3,19V15A2,2 0 0,0 1,13H0V11H1A2,2 0 0,0 3,9V5A2,2 0 0,1 5,3M19,3A2,2 0 0,1 21,5V9A2,2 0 0,0 23,11H24V13H23A2,2 0 0,0 21,15V19A2,2 0 0,1 19,21H17V19H19V14A2,2 0 0,1 21,12A2,2 0 0,1 19,10V5H17V3H19M12,15A1,1 0 0,1 13,16A1,1 0 0,1 12,17A1,1 0 0,1 11,16A1,1 0 0,1 12,15M8,15A1,1 0 0,1 9,16A1,1 0 0,1 8,17A1,1 0 0,1 7,16A1,1 0 0,1 8,15M16,15A1,1 0 0,1 17,16A1,1 0 0,1 16,17A1,1 0 0,1 15,16A1,1 0 0,1 16,15Z"
        , title =
            case style of
                Active ->
                    "Hide model"

                _ ->
                    "Show model"
        }


viewPlayIcon : Style -> Element msg
viewPlayIcon style =
    viewIcon
        { viewBox = ViewBox 0 0 24 24
        , style = style
        , path = "M8,5.14V19.14L19,12.14L8,5.14Z"
        , title =
            case style of
                Active ->
                    "Pause app"

                _ ->
                    "Start app"
        }


viewImportIcon : Style -> Element msg
viewImportIcon style =
    viewIcon
        { viewBox = ViewBox 0 0 24 24
        , style = style
        , path = "M9,16V10H5L12,3L19,10H15V16H9M5,20V18H19V20H5Z"
        , title =
            case style of
                Error error ->
                    error

                _ ->
                    "Load session"
        }


viewExportIcon : Style -> Element msg
viewExportIcon style =
    viewIcon
        { viewBox = ViewBox 0 0 24 24
        , style = style
        , path = "M5,20H19V18H5M19,9H15V3H9V9H5L12,16L19,9Z"
        , title = "Save session"
        }


viewResetIcon : Style -> Element msg
viewResetIcon style =
    viewIcon
        { viewBox = ViewBox 0 0 24 24
        , style = style
        , path = "M12,4C14.1,4 16.1,4.8 17.6,6.3C20.7,9.4 20.7,14.5 17.6,17.6C15.8,19.5 13.3,20.2 10.9,19.9L11.4,17.9C13.1,18.1 14.9,17.5 16.2,16.2C18.5,13.9 18.5,10.1 16.2,7.7C15.1,6.6 13.5,6 12,6V10.6L7,5.6L12,0.6V4M6.3,17.6C3.7,15 3.3,11 5.1,7.9L6.6,9.4C5.5,11.6 5.9,14.4 7.8,16.2C8.3,16.7 8.9,17.1 9.6,17.4L9,19.4C8,19 7.1,18.4 6.3,17.6Z"
        , title = "Reset app"
        }
