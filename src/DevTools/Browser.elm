module DevTools.Browser exposing (application, document, element, sandbox)

import Browser
import Browser.Navigation
import DevTools.Browser.Program as Program exposing (Program)
import Html exposing (Html)
import Json.Decode as Jd
import Json.Encode as Je
import Url exposing (Url)


type alias Config flags model msg =
    { printModel : model -> String
    , encodeMsg : msg -> Je.Value
    , msgDecoder : Jd.Decoder msg
    , fromCache : flags -> Maybe String
    , toCache : Je.Value -> Cmd (Program.Msg model msg)
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
                Program.mapInit
                    { update = \msg model -> ( update msg model, Cmd.none )
                    , msgDecoder = devTools.msgDecoder
                    , init = ( init, Cmd.none )
                    , fromCache = devTools.fromCache flags
                    }
        , view =
            Program.mapDocument
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , viewApp = \model -> { title = "", body = view model :: [] }
                , update = \msg model -> ( update msg model, Cmd.none )
                }
        , update =
            Program.mapUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , toCache = devTools.toCache
                , update = \msg model -> ( update msg model, Cmd.none )
                }
        , subscriptions =
            Program.mapSubscriptions
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
                Program.mapInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , init = init flags
                    , fromCache = devTools.fromCache flags
                    }
        , view =
            Program.mapHtml
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , viewApp = view
                , update = update
                }
        , update =
            Program.mapUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , toCache = devTools.toCache
                , update = update
                }
        , subscriptions =
            Program.mapSubscriptions
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
                Program.mapInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , init = init flags
                    , fromCache = devTools.fromCache flags
                    }
        , view =
            Program.mapDocument
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , viewApp = view
                , update = update
                }
        , update =
            Program.mapUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , update = update
                , toCache = devTools.toCache
                }
        , subscriptions =
            Program.mapSubscriptions
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
                Program.mapInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , init = init flags url key
                    , fromCache = devTools.fromCache flags
                    }
        , view =
            Program.mapDocument
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , viewApp = view
                , update = update
                }
        , update =
            Program.mapUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , update = update
                , toCache = devTools.toCache
                }
        , subscriptions =
            Program.mapSubscriptions
                { msgDecoder = devTools.msgDecoder
                , subscriptions = subscriptions
                , update = update
                }
        , onUrlChange = Program.mapUrlMsg << onUrlChange
        , onUrlRequest = Program.mapUrlMsg << onUrlRequest
        }
