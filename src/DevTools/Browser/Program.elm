module DevTools.Browser.Program exposing
    ( Model
    , Msg
    , Program
    , mapDocument
    , mapHtml
    , mapInit
    , mapSubscriptions
    , mapUpdate
    , mapUrlMsg
    )

import Browser
import History exposing (History)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode


type alias Program flags model msg =
    Platform.Program flags (Model model msg) (Msg model msg)


type Msg model msg
    = DoNothing
    | UpdateApp MsgSrc msg
    | ResetApp
    | ReplayApp Int


type alias Model model msg =
    { history : History model msg
    , initCmd : Cmd msg
    }


mapUrlMsg : msg -> Msg model msg
mapUrlMsg =
    UpdateApp Url


mapInit :
    { init : ( model, Cmd msg )
    , msgDecoder : Json.Decode.Decoder msg
    , update : msg -> model -> ( model, Cmd msg )
    , fromCache : Maybe Json.Encode.Value
    }
    -> ( Model model msg, Cmd (Msg model msg) )
mapInit config =
    ( { history = History.init (Tuple.first config.init)
      , initCmd = Tuple.second config.init
      }
    , Cmd.map (UpdateApp Init) (Tuple.second config.init)
    )


mapSubscriptions :
    { msgDecoder : Json.Decode.Decoder msg
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> Sub (Msg model msg)
mapSubscriptions config model =
    Sub.map (UpdateApp Subs) (config.subscriptions (History.currentModel model.history))


mapUpdate :
    { msgDecoder : Json.Decode.Decoder msg
    , encodeMsg : msg -> Json.Encode.Value
    , update : msg -> model -> ( model, Cmd msg )
    , toCache : Json.Encode.Value -> Cmd (Msg model msg)
    }
    -> Msg model msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg model msg) )
mapUpdate config msg model =
    case msg of
        DoNothing ->
            ( model
            , Cmd.none
            )

        UpdateApp src appMsg ->
            ( { model | history = recordFromSrc src (withoutCmd config.update) appMsg model.history }
            , model.history
                |> History.currentModel
                |> config.update appMsg
                |> Tuple.second
                |> Cmd.map (UpdateApp Update)
            )

        ResetApp ->
            ( { model | history = History.reset model.history }
            , Cmd.map (UpdateApp Init) model.initCmd
            )

        ReplayApp index ->
            ( { model | history = History.replay (withoutCmd config.update) index model.history }
            , Cmd.none
            )


mapDocument :
    { encodeMsg : msg -> Json.Encode.Value
    , printModel : model -> String
    , viewApp : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> Browser.Document (Msg model msg)
mapDocument config model =
    let
        { title, body } =
            config.viewApp (History.currentModel model.history)
    in
    { title = title
    , body =
        viewResetButton
            :: viewReplaySlider model.history
            :: List.map (Html.map (UpdateApp View)) body
    }


mapHtml :
    { encodeMsg : msg -> Json.Encode.Value
    , printModel : model -> String
    , viewApp : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> Model model msg
    -> Html (Msg model msg)
mapHtml config model =
    Html.map (UpdateApp View) (config.viewApp (History.currentModel model.history))



-- MsgSrc


type MsgSrc
    = Init
    | Update
    | Subs
    | View
    | Url


recordFromSrc :
    MsgSrc
    -> (msg -> model -> model)
    -> msg
    -> History model msg
    -> History model msg
recordFromSrc src =
    case src of
        Init ->
            History.recordForever

        _ ->
            History.record



-- Helpers


withoutCmd : (msg -> model -> ( model, Cmd msg )) -> msg -> model -> model
withoutCmd update msg model =
    Tuple.first (update msg model)


viewResetButton : Html (Msg model msg)
viewResetButton =
    Html.button
        [ Html.Events.onClick ResetApp
        ]
        [ Html.text "Reset"
        ]


viewReplaySlider : History model msg -> Html (Msg model msg)
viewReplaySlider history =
    Html.input
        [ Html.Attributes.type_ "range"
        , Html.Attributes.step (String.fromInt 1)
        , Html.Attributes.min (String.fromInt 0)
        , Html.Attributes.max (String.fromInt (History.length history))
        , Html.Attributes.value (String.fromInt (History.currentIndex history))
        , Html.Events.onInput (Maybe.withDefault DoNothing << Maybe.map ReplayApp << String.toInt)
        ]
        []
