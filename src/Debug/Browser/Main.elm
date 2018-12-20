module Debug.Browser.Main exposing
    ( Configuration
    , Program
    , wrapDocument
    , wrapHtml
    , wrapInit
    , wrapMsg
    , wrapSubscriptions
    , wrapUpdate
    )

import Browser
import Debug.Browser.History as History exposing (History)
import Html exposing (Html)
import Json.Decode
import Json.Encode


type alias Configuration model msg =
    { printModel : model -> String
    , encodeMsg : msg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder msg
    , toPort : Json.Encode.Value -> Cmd (Msg msg)
    }


type alias Program model msg =
    Platform.Program Json.Decode.Value (Model model msg) (Msg msg)


type alias Model model msg =
    { history : History model msg
    }


type Msg msg
    = Update msg


mapUpdate : Cmd msg -> Cmd (Msg msg)
mapUpdate =
    Cmd.map Update


wrapMsg : (a -> msg) -> a -> Msg msg
wrapMsg toMsg =
    Update << toMsg


wrapInit :
    { model : model
    , cmds : Cmd msg
    , flags : Json.Decode.Value
    , msgDecoder : Json.Decode.Decoder msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> ( Model model msg, Cmd (Msg msg) )
wrapInit { update, msgDecoder, flags, model, cmds } =
    ( { history = History.init model }
    , mapUpdate cmds
    )


wrapSubscriptions :
    { msgDecoder : Json.Decode.Decoder msg
    , subscriptions : model -> Sub msg
    }
    -> Model model msg
    -> Sub (Msg msg)
wrapSubscriptions { msgDecoder, subscriptions } model =
    Sub.map Update (subscriptions (History.now model.history))


wrapUpdate :
    { msgDecoder : Json.Decode.Decoder msg
    , encodeMsg : msg -> Json.Encode.Value
    , update : msg -> model -> ( model, Cmd msg )
    , toPort : Json.Encode.Value -> Cmd (Msg msg)
    }
    -> Msg msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg msg) )
wrapUpdate { msgDecoder, encodeMsg, update, toPort } msg model =
    case msg of
        Update updateMsg ->
            let
                ( updatedModel, updateCmds ) =
                    update updateMsg (History.now model.history)
            in
            ( { model
                | history = History.insert ( updateMsg, updatedModel ) model.history
              }
            , mapUpdate updateCmds
            )


wrapDocument :
    { encodeMsg : msg -> Json.Encode.Value
    , printModel : model -> String
    , view : model -> Browser.Document msg
    }
    -> Model model msg
    -> Browser.Document (Msg msg)
wrapDocument { printModel, encodeMsg, view } model =
    let
        { title, body } =
            view (History.now model.history)
    in
    { title = title
    , body = List.map (Html.map Update) body
    }


wrapHtml :
    { encodeMsg : msg -> Json.Encode.Value
    , printModel : model -> String
    , view : model -> Html msg
    }
    -> Model model msg
    -> Html (Msg msg)
wrapHtml { printModel, encodeMsg, view } model =
    Html.map Update (view (History.now model.history))
