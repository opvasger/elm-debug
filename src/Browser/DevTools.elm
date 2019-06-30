module Browser.DevTools exposing (application, document, element, sandbox)

import Browser
import Browser.DevTools.Main as Main exposing (Program)
import Browser.Navigation
import Html exposing (Html)
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import Url exposing (Url)


type alias Config flags model msg =
    { encodeModel : model -> Encode.Value
    , encodeMsg : msg -> Encode.Value
    , msgDecoder : Decoder msg
    , fromCache : flags -> Maybe String
    , toCache : String -> Cmd (Main.Msg model msg)
    }


sandbox :
    { init : model
    , view : model -> Html msg
    , update : msg -> model -> model
    , devTools : Config flags model msg
    }
    -> Program flags model msg
sandbox { init, view, update, devTools } =
    Browser.document
        { init =
            \flags ->
                Main.toInit
                    { update = \msg model -> ( update msg model, Cmd.none )
                    , msgDecoder = devTools.msgDecoder
                    , init = ( init, Cmd.none )
                    , fromCache = devTools.fromCache flags
                    }
        , view =
            Main.toDocument
                { encodeModel = devTools.encodeModel
                , encodeMsg = devTools.encodeMsg
                , view = \model -> { title = "", body = [ view model ] }
                , update = \msg model -> ( update msg model, Cmd.none )
                }
        , update =
            Main.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , toCache = devTools.toCache
                , update = \msg model -> ( update msg model, Cmd.none )
                }
        , subscriptions =
            Main.toSubscriptions
                { subscriptions = always Sub.none
                , msgDecoder = devTools.msgDecoder
                , update = \msg model -> ( update msg model, Cmd.none )
                }
        }


element :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , devTools : Config flags model msg
    }
    -> Program flags model msg
element { init, view, update, subscriptions, devTools } =
    Browser.element
        { init =
            \flags ->
                Main.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , init = init flags
                    , fromCache = devTools.fromCache flags
                    }
        , view =
            Main.toHtml
                { encodeModel = devTools.encodeModel
                , encodeMsg = devTools.encodeMsg
                , view = view
                , update = update
                }
        , update =
            Main.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , toCache = devTools.toCache
                , update = update
                }
        , subscriptions =
            Main.toSubscriptions
                { msgDecoder = devTools.msgDecoder
                , subscriptions = subscriptions
                , update = update
                }
        }


document :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , devTools : Config flags model msg
    }
    -> Program flags model msg
document { init, view, update, subscriptions, devTools } =
    Browser.document
        { init =
            \flags ->
                Main.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , init = init flags
                    , fromCache = devTools.fromCache flags
                    }
        , view =
            Main.toDocument
                { encodeModel = devTools.encodeModel
                , encodeMsg = devTools.encodeMsg
                , view = view
                , update = update
                }
        , update =
            Main.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , update = update
                , toCache = devTools.toCache
                }
        , subscriptions =
            Main.toSubscriptions
                { msgDecoder = devTools.msgDecoder
                , subscriptions = subscriptions
                , update = update
                }
        }


application :
    { init : flags -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url -> msg
    , devTools : Config flags model msg
    }
    -> Program flags model msg
application { init, view, update, subscriptions, onUrlRequest, onUrlChange, devTools } =
    Browser.application
        { init =
            \flags url key ->
                Main.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , init = init flags url key
                    , fromCache = devTools.fromCache flags
                    }
        , view =
            Main.toDocument
                { encodeModel = devTools.encodeModel
                , encodeMsg = devTools.encodeMsg
                , view = view
                , update = update
                }
        , update =
            Main.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , update = update
                , toCache = devTools.toCache
                }
        , subscriptions =
            Main.toSubscriptions
                { msgDecoder = devTools.msgDecoder
                , subscriptions = subscriptions
                , update = update
                }
        , onUrlChange = Main.toUrlMsg << onUrlChange
        , onUrlRequest = Main.toUrlMsg << onUrlRequest
        }
