module LazyViewport exposing (Configuration, viewScene)

import Browser as B
import Html as H exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import Json.Decode as Jd


type alias Configuration a msg =
    { visibleCount : Int
    , itemHeight : Int
    , focusedIndex : Int
    , onFocus : Int -> msg
    , selectedIndex : Int
    , onSelect : Int -> a -> msg
    , viewItem : Bool -> a -> Html msg
    , items : List a
    }


viewScene : Configuration a msg -> Html msg
viewScene { onFocus, onSelect, viewItem, items, visibleCount, itemHeight, focusedIndex, selectedIndex } =
    let
        itemLength =
            List.length items

        halfVisibleCount =
            visibleCount // 2

        dropCount =
            clamp 0 (itemLength - visibleCount) (focusedIndex - halfVisibleCount)
    in
    H.div
        [ Ha.style "overflow" "hidden scroll"
        , Ha.style "border" "1px solid #e7e7e7"
        , Ha.style "border-right" "none"
        , Ha.style "height" (String.fromInt (visibleCount * itemHeight) ++ "px")
        , He.on "scroll"
            (Jd.map
                (\scrollTop -> onFocus (round scrollTop // itemHeight + halfVisibleCount))
                (Jd.at [ "target", "scrollTop" ] Jd.float)
            )
        ]
        [ H.div
            [ Ha.style "height" (String.fromInt (itemLength * itemHeight) ++ "px")
            ]
            [ H.div
                [ Ha.style "padding-top" (String.fromInt (dropCount * itemHeight) ++ "px") ]
                (List.indexedMap
                    (\relativeIndex item ->
                        H.div
                            [ Ha.style "height" (String.fromInt itemHeight ++ "px")
                            , He.onClick (onSelect (relativeIndex + dropCount) item)
                            ]
                            [ viewItem (selectedIndex == relativeIndex + dropCount) item
                            ]
                    )
                    (List.take (visibleCount + 1) (List.drop dropCount items))
                )
            ]
        ]
