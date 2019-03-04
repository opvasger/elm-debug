module DevTools.Browser exposing (application, document, element, sandbox)

import Browser
import Browser.Navigation
import DevTools
import Html exposing (Html)
import Json.Decode as Jd
import Url exposing (Url)


sandbox :
    { init : model
    , view : model -> Html msg
    , update : msg -> model -> model
    , devTools : DevTools.Config model msg
    }
    -> DevTools.Program Jd.Value model msg
sandbox { init, view, update, devTools } =
    Browser.document
        { init =
            \flags ->
                DevTools.toInit
                    { update = \msg model -> ( update msg model, Cmd.none )
                    , msgDecoder = devTools.msgDecoder
                    , flags = flags
                    , modelCmdPair = ( init, Cmd.none )
                    }
        , view =
            DevTools.toDocument
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , view = \model -> { title = "", body = view model :: [] }
                }
        , update =
            DevTools.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , output = devTools.output
                , update = \msg model -> ( update msg model, Cmd.none )
                }
        , subscriptions =
            DevTools.toSubscriptions
                { subscriptions = always Sub.none
                , msgDecoder = devTools.msgDecoder
                }
        }


element :
    { init : Jd.Value -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , devTools : DevTools.Config model msg
    }
    -> DevTools.Program Jd.Value model msg
element { init, view, update, subscriptions, devTools } =
    Browser.element
        { init =
            \flags ->
                DevTools.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , flags = flags
                    , modelCmdPair = init flags
                    }
        , view =
            DevTools.toHtml
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , view = view
                }
        , update =
            DevTools.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , output = devTools.output
                , update = update
                }
        , subscriptions =
            DevTools.toSubscriptions
                { msgDecoder = devTools.msgDecoder
                , subscriptions = subscriptions
                }
        }


document :
    { init : Jd.Value -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , devTools : DevTools.Config model msg
    }
    -> DevTools.Program Jd.Value model msg
document { init, view, update, subscriptions, devTools } =
    Browser.document
        { init =
            \flags ->
                DevTools.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , flags = flags
                    , modelCmdPair = init flags
                    }
        , view =
            DevTools.toDocument
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , view = view
                }
        , update =
            DevTools.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , update = update
                , output = devTools.output
                }
        , subscriptions =
            DevTools.toSubscriptions
                { subscriptions = subscriptions
                , msgDecoder = devTools.msgDecoder
                }
        }


application :
    { init : Jd.Value -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url -> msg
    , devTools : DevTools.Config model msg
    }
    -> DevTools.Program Jd.Value model msg
application { init, view, update, subscriptions, onUrlRequest, onUrlChange, devTools } =
    Browser.application
        { init =
            \flags url key ->
                DevTools.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , flags = flags
                    , modelCmdPair = init flags url key
                    }
        , view =
            DevTools.toDocument
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , view = view
                }
        , update =
            DevTools.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , update = update
                , output = devTools.output
                }
        , subscriptions =
            DevTools.toSubscriptions
                { subscriptions = subscriptions
                , msgDecoder = devTools.msgDecoder
                }
        , onUrlChange = DevTools.toMsg << onUrlChange
        , onUrlRequest = DevTools.toMsg << onUrlRequest
        }
