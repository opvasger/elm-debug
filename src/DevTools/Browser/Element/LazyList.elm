module DevTools.Browser.Element.LazyList exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (style)


view :
    { viewElement : a -> Html msg
    , queryElements : Int -> Int -> List a
    , length : Int
    }
    -> Html msg
view config =
    div
        [ style "padding" "4px"
        ]
        (List.map config.viewElement (config.queryElements (config.length - 9) config.length))
