module Form exposing
    ( encodeMsg
    , fromCache
    , init
    , msgDecoder
    , subscriptions
    , update
    , view
    )

import Browser
import Html as H exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import Json.Decode as Jd
import Json.Encode as Je


type alias Flags =
    { devTools : Maybe Jd.Value
    }


fromCache : Flags -> Maybe Jd.Value
fromCache =
    .devTools


type alias Model =
    { page : Page
    }


type Msg
    = InputName String
    | InputPass String
    | LogIn
    | LogOut
    | Increment
    | Decrement


type Error
    = NoError
    | NoName
    | NoPass


type alias AuthState =
    { name : String
    , pass : String
    , err : Error
    }


type alias CountState =
    { name : String
    , count : Int
    }


type Page
    = Auth AuthState
    | Count CountState


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { page = initAuth }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.page of
        Auth state ->
            case msg of
                InputName name ->
                    ( { model | page = Auth { state | name = name } }, Cmd.none )

                InputPass pass ->
                    ( { model | page = Auth { state | pass = pass } }, Cmd.none )

                LogIn ->
                    case authenticate state of
                        Just err ->
                            ( { model | page = Auth { state | err = err } }, Cmd.none )

                        Nothing ->
                            ( { model | page = initCount state.name }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Count state ->
            case msg of
                LogOut ->
                    ( { model | page = initAuth }, Cmd.none )

                Increment ->
                    ( { model | page = Count { state | count = state.count + 1 } }, Cmd.none )

                Decrement ->
                    ( { model | page = Count { state | count = state.count - 1 } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Example"
    , body =
        [ viewPage model.page
        ]
    }


authenticate : AuthState -> Maybe Error
authenticate { name, pass } =
    if String.length (String.trim name) < 1 then
        Just NoName

    else if String.length (String.trim pass) < 1 then
        Just NoPass

    else
        Nothing


errorToString : Error -> String
errorToString err =
    case err of
        NoError ->
            ""

        NoName ->
            "please specify a name"

        NoPass ->
            "please specify a password"


initAuth : Page
initAuth =
    Auth { name = "", pass = "", err = NoError }


initCount : String -> Page
initCount name =
    Count { name = name, count = 0 }


msgDecoder : Jd.Decoder Msg
msgDecoder =
    Jd.oneOf
        [ Jd.map InputName (Jd.field "Input Name" Jd.string)
        , Jd.map InputPass (Jd.field "Input Pass" Jd.string)
        , Jd.field "Log In" (Jd.null LogIn)
        , Jd.field "Log Out" (Jd.null LogOut)
        , Jd.field "Increment" (Jd.null Increment)
        , Jd.field "Decrement" (Jd.null Decrement)
        ]


encodeMsg : Msg -> Je.Value
encodeMsg msg =
    case msg of
        InputName name ->
            Je.object [ ( "Input Name", Je.string name ) ]

        InputPass pass ->
            Je.object [ ( "Input Pass", Je.string pass ) ]

        LogIn ->
            Je.object [ ( "Log In", Je.null ) ]

        LogOut ->
            Je.object [ ( "Log Out", Je.null ) ]

        Increment ->
            Je.object [ ( "Increment", Je.null ) ]

        Decrement ->
            Je.object [ ( "Decrement", Je.null ) ]


centerAttributes : List (H.Attribute Msg)
centerAttributes =
    [ Ha.style "height" "95vh"
    , Ha.style "display" "flex"
    , Ha.style "flex-direction" "column"
    , Ha.style "align-items" "center"
    , Ha.style "justify-content" "center"
    ]


viewPage : Page -> Html Msg
viewPage page =
    case page of
        Auth state ->
            viewAuth state

        Count state ->
            viewCount state


viewAuth : AuthState -> Html Msg
viewAuth { name, pass, err } =
    H.div
        centerAttributes
        [ H.input
            [ Ha.type_ "text"
            , Ha.placeholder "name"
            , He.onInput InputName
            , Ha.value name
            ]
            []
        , H.input
            [ Ha.type_ "password"
            , Ha.placeholder "pass"
            , He.onInput InputPass
            , Ha.value pass
            ]
            []
        , H.button
            [ He.onClick LogIn
            ]
            [ H.text "Log In"
            ]
        , H.text (errorToString err)
        ]


viewCount : CountState -> Html Msg
viewCount { name, count } =
    H.div centerAttributes
        [ H.text ("Hello " ++ name)
        , H.button
            [ He.onClick LogOut
            ]
            [ H.text "Log Out"
            ]
        , H.div []
            [ H.button
                [ He.onClick Increment
                ]
                [ H.text "+"
                ]
            , H.text (String.fromInt count)
            , H.button
                [ He.onClick Decrement
                ]
                [ H.text "-"
                ]
            ]
        ]
