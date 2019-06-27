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
    { devTools : Maybe String
    }


fromCache : Flags -> Maybe String
fromCache =
    .devTools


type alias Model =
    Int


type Msg
    = Increment
    | Decrement
    | Reset


msgDecoder : Jd.Decoder Msg
msgDecoder =
    Jd.oneOf
        [ Jd.field "Increment" (Jd.null Increment)
        , Jd.field "Decrement" (Jd.null Decrement)
        , Jd.field "Reset" (Jd.null Reset)
        ]


encodeMsg : Msg -> Je.Value
encodeMsg msg =
    case msg of
        Increment ->
            Je.object [ ( "Increment", Je.null ) ]

        Decrement ->
            Je.object [ ( "Decrement", Je.null ) ]

        Reset ->
            Je.object [ ( "Reset", Je.null ) ]


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( 0
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

        Decrement ->
            ( model - 1, Cmd.none )

        Reset ->
            ( 0, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Example"
    , body =
        [ H.button [ He.onClick Increment ] [ H.text "+" ]
        , H.button [ He.onClick Reset ] [ H.text (String.fromInt model) ]
        , H.button [ He.onClick Decrement ] [ H.text "-" ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
