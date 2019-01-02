module List.Infinite exposing (Model, Msg, update, view)

import Browser as B
import Html as H exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import Json.Decode as Jd


type alias Model =
    { focusedIndex : Int
    , selectedIndex : Int
    , visibleLength : Int
    , itemHeight : Int
    }


type Msg
    = FocusIndex Int
    | SelectIndex Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusIndex focusedIndex ->
            ( { model | focusedIndex = focusedIndex }, Cmd.none )

        SelectIndex selectedIndex ->
            ( { model | selectedIndex = selectedIndex }, Cmd.none )


view : Model -> (Bool -> a -> Html Msg) -> List a -> Html Msg
view { focusedIndex, selectedIndex, visibleLength, itemHeight } viewItem items =
    let
        indexOffset =
            clamp 0 (List.length items - visibleLength) (focusedIndex - visibleLength // 2)

        visibleItems =
            List.take (visibleLength + 1) (List.drop indexOffset items)

        viewVisibleItem =
            wrapViewItem viewItem itemHeight selectedIndex indexOffset
    in
    H.div
        [ Ha.style "overflow" "hidden scroll"
        , Ha.style "border" "1px solid #e7e7e7"
        , Ha.style "border-right" "none"
        , Ha.style "height" (String.fromInt (visibleLength * itemHeight) ++ "px")
        , He.on "scroll" (scrollFocusDecoder itemHeight visibleLength)
        ]
        [ H.div
            [ Ha.style "height" (String.fromInt (List.length items * itemHeight) ++ "px")
            ]
            [ H.div
                [ Ha.style "padding-top" (String.fromInt (indexOffset * itemHeight) ++ "px") ]
                (List.indexedMap viewVisibleItem visibleItems)
            ]
        ]


scrollFocusDecoder : Int -> Int -> Jd.Decoder Msg
scrollFocusDecoder itemHeight visibleLength =
    Jd.map
        (\scrollTop -> FocusIndex (round scrollTop // itemHeight + visibleLength // 2))
        (Jd.at [ "target", "scrollTop" ] Jd.float)


wrapViewItem : (Bool -> a -> Html Msg) -> Int -> Int -> Int -> Int -> a -> Html Msg
wrapViewItem viewItem itemHeight selectedIndex indexOffset relativeIndex item =
    let
        index =
            relativeIndex + indexOffset
    in
    H.div
        [ Ha.style "height" (String.fromInt itemHeight ++ "px")
        , He.onClick (SelectIndex index)
        ]
        [ viewItem (selectedIndex == index) item
        ]


main : Program () Model Msg
main =
    B.document
        { init = always ( { selectedIndex = 0, focusedIndex = 0, visibleLength = 5, itemHeight = 20 }, Cmd.none )
        , update = update
        , view =
            \model ->
                { title = "example"
                , body =
                    [ view
                        model
                        (\isSelected n ->
                            H.div
                                [ if isSelected then
                                    Ha.style "background-color" "#fafafa"

                                  else
                                    Ha.style "" ""
                                ]
                                [ H.text (String.fromInt n)
                                ]
                        )
                        (List.range 0 1000)
                    ]
                }
        , subscriptions = \model -> Sub.none
        }
