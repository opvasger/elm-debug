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
import Debug.History as History exposing (History)
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


wrapMsg : (a -> msg) -> a -> Msg msg
wrapMsg toMsg =
    Update << toMsg


wrapInit :
    { modelCmdPair : ( model, Cmd msg )
    , flags : Json.Decode.Value
    , msgDecoder : Json.Decode.Decoder msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> ( Model model msg, Cmd (Msg msg) )
wrapInit { update, msgDecoder, flags, modelCmdPair } =
    Debug.todo "..."


wrapSubscriptions :
    { msgDecoder : Json.Decode.Decoder msg
    , subscriptions : model -> Sub msg
    }
    -> Model model msg
    -> Sub (Msg msg)
wrapSubscriptions { msgDecoder, subscriptions } =
    Debug.todo "..."


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
    Debug.todo "..."


wrapDocument :
    { encodeMsg : msg -> Json.Encode.Value
    , printModel : model -> String
    , toDocument : model -> Browser.Document msg
    }
    -> Model model msg
    -> Browser.Document (Msg msg)
wrapDocument { printModel, encodeMsg, toDocument } model =
    Debug.todo "..."


wrapHtml :
    { encodeMsg : msg -> Json.Encode.Value
    , printModel : model -> String
    , toHtml : model -> Html msg
    }
    -> Model model msg
    -> Html (Msg msg)
wrapHtml { printModel, encodeMsg, toHtml } model =
    Debug.todo "..."
