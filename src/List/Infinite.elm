module List.Infinite exposing (Model, Msg, update, view)

import Browser as B
import Html as H exposing (Html)
import Html.Attributes as Ha
import Html.Events as He


type alias Model =
    { selectedIndex : Int
    , visibleLength : Int
    }


type Msg
    = SelectIndex Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectIndex selectedIndex ->
            ( { model | selectedIndex = selectedIndex }, Cmd.none )


view : Model -> (Bool -> a -> Html Msg) -> List a -> Html Msg
view { selectedIndex, visibleLength } viewItem items =
    let
        indexOffset =
            clamp 0 (List.length items - visibleLength) (selectedIndex - visibleLength // 2)

        visibleItems =
            List.take visibleLength (List.drop indexOffset items)

        viewVisibleItem =
            wrapViewItem viewItem selectedIndex indexOffset
    in
    H.div
        [ Ha.style "overflow" "hidden scroll"
        , Ha.style "border" "1px solid #e7e7e7"
        , Ha.style "border-right" "none"
        , Ha.style "height" (String.fromInt (visibleLength * 20) ++ "px")
        ]
        [ viewWithHeight (20 * indexOffset)
        , H.div [] (List.indexedMap viewVisibleItem visibleItems)
        , viewWithHeight (20 * (List.length items - indexOffset - visibleLength))
        ]


viewWithHeight : Int -> Html Msg
viewWithHeight height =
    H.div [ Ha.style "height" (String.fromInt height ++ "px") ] []


wrapViewItem : (Bool -> a -> Html Msg) -> Int -> Int -> Int -> a -> Html Msg
wrapViewItem viewItem selectedIndex indexOffset relativeIndex item =
    let
        index =
            relativeIndex + indexOffset
    in
    H.div
        [ He.onClick (SelectIndex index)
        ]
        [ viewItem (selectedIndex == index) item
        ]


main : Program () Model Msg
main =
    B.document
        { init = always ( { selectedIndex = 0, visibleLength = 5 }, Cmd.none )
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
                        [ 0
                        , 1
                        , 2
                        , 3
                        , 4
                        , 5
                        , 6
                        , 7
                        , 8
                        , 9
                        , 10
                        , 11
                        , 12
                        , 13
                        , 14
                        , 15
                        , 16
                        , 17
                        , 18
                        , 19
                        ]
                    ]
                }
        , subscriptions = \model -> Sub.none
        }
