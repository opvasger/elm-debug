module DevTools exposing (Config, Program, toDocument, toHtml, toInit, toMsg, toSubscriptions, toUpdate)

import Browser
import History exposing (History)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Jd
import Json.Encode as Je


type alias Model model msg =
    { history : History model msg
    }


type Msg msg
    = AppMsg msg
    | InitialAppMsg msg
    | SliderInput Int
    | ToggleReplay


type alias Config flags model msg =
    { printModel : model -> String
    , encodeMsg : msg -> Je.Value
    , msgDecoder : Jd.Decoder msg
    , toSession : flags -> Maybe String
    , output : Je.Value -> Cmd (Msg msg)
    }


type alias Program flags model msg =
    Platform.Program flags (Model model msg) (Msg msg)


toMsg : msg -> Msg msg
toMsg =
    AppMsg


toDocument :
    { encodeMsg : msg -> Je.Value
    , printModel : model -> String
    , view : model -> Browser.Document msg
    }
    -> Model model msg
    -> Browser.Document (Msg msg)
toDocument config model =
    let
        { title, body } =
            config.view (History.currentModel model.history)
    in
    { title = title
    , body = view model :: List.map (Html.map AppMsg) body
    }


toHtml :
    { encodeMsg : msg -> Je.Value
    , printModel : model -> String
    , view : model -> Html msg
    }
    -> Model model msg
    -> Html (Msg msg)
toHtml config model =
    Html.div []
        [ view model
        , Html.map AppMsg (config.view (History.currentModel model.history))
        ]


toInit :
    { modelCmdPair : ( model, Cmd msg )
    , msgDecoder : Jd.Decoder msg
    , update : msg -> model -> ( model, Cmd msg )
    , session : Maybe String
    }
    -> ( Model model msg, Cmd (Msg msg) )
toInit config =
    ( { history = History.init (Tuple.first config.modelCmdPair)
      }
    , Cmd.map InitialAppMsg (Tuple.second config.modelCmdPair)
    )


toSubscriptions :
    { msgDecoder : Jd.Decoder msg
    , subscriptions : model -> Sub msg
    }
    -> Model model msg
    -> Sub (Msg msg)
toSubscriptions config model =
    if History.isReplaying model.history then
        Sub.none

    else
        Sub.map AppMsg (config.subscriptions (History.currentModel model.history))


toUpdate :
    { msgDecoder : Jd.Decoder msg
    , encodeMsg : msg -> Je.Value
    , update : msg -> model -> ( model, Cmd msg )
    , output : Je.Value -> Cmd (Msg msg)
    }
    -> Msg msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg msg) )
toUpdate config msg model =
    case msg of
        AppMsg appMsg ->
            let
                ( history, cmd ) =
                    History.update config.update appMsg model.history
            in
            ( { model | history = history }, Cmd.map AppMsg cmd )

        InitialAppMsg appMsg ->
            let
                ( history, cmd ) =
                    History.updateAndPersist config.update appMsg model.history
            in
            ( { model | history = history }, Cmd.map AppMsg cmd )

        SliderInput index ->
            ( { model
                | history = History.replay config.update index model.history
              }
            , Cmd.none
            )

        ToggleReplay ->
            ( { model | history = History.toggleState config.update model.history }, Cmd.none )


view : Model model msg -> Html (Msg msg)
view model =
    Html.div []
        [ Html.button [ Events.onClick ToggleReplay ] [ Html.text "Toggle" ]
        , Html.input
            [ Attr.type_ "range"
            , Attr.min "0"
            , Attr.max (String.fromInt (History.length model.history))
            , Attr.value
                (String.fromInt (History.currentIndex model.history))
            , Events.onInput (SliderInput << Maybe.withDefault (History.currentIndex model.history) << String.toInt)
            ]
            []
        ]
