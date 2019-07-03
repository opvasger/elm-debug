module Range exposing
    ( Model
    , Msg
    , decoder
    , encode
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Msg
    = Msg


type alias Model =
    {}


init : Model
init =
    {}


update : Msg -> Model -> Model
update msg model =
    case msg of
        Msg ->
            model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div
        [ style "width" "100%"
        ]
        []



-- Json


encode : Model -> Encode.Value
encode model =
    Encode.object
        []


decoder : Decoder Model
decoder =
    Decode.succeed {}
