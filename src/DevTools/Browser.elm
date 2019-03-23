module DevTools.Browser exposing (application, document, element, sandbox)

import Browser
import Browser.Navigation
import DevTools
import Html exposing (Html)
import Url exposing (Url)


sandbox :
    { init : model
    , view : model -> Html msg
    , update : msg -> model -> model
    , devTools : DevTools.Config flags model msg
    }
    -> DevTools.Program flags model msg
sandbox { init, view, update, devTools } =
    Browser.document
        { init =
            \flags ->
                DevTools.toInit
                    { update = \msg model -> ( update msg model, Cmd.none )
                    , msgDecoder = devTools.msgDecoder
                    , modelCmdPair = ( init, Cmd.none )
                    , session = devTools.toSession flags
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
        , subscriptions = DevTools.toSubscriptions (always Sub.none)
        }


element :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , devTools : DevTools.Config flags model msg
    }
    -> DevTools.Program flags model msg
element { init, view, update, subscriptions, devTools } =
    Browser.element
        { init =
            \flags ->
                DevTools.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , modelCmdPair = init flags
                    , session = devTools.toSession flags
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
        , subscriptions = DevTools.toSubscriptions subscriptions
        }


document :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , devTools : DevTools.Config flags model msg
    }
    -> DevTools.Program flags model msg
document { init, view, update, subscriptions, devTools } =
    Browser.document
        { init =
            \flags ->
                DevTools.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , modelCmdPair = init flags
                    , session = devTools.toSession flags
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
        , subscriptions = DevTools.toSubscriptions subscriptions
        }


application :
    { init : flags -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url -> msg
    , devTools : DevTools.Config flags model msg
    }
    -> DevTools.Program flags model msg
application { init, view, update, subscriptions, onUrlRequest, onUrlChange, devTools } =
    Browser.application
        { init =
            \flags url key ->
                DevTools.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , modelCmdPair = init flags url key
                    , session = devTools.toSession flags
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
        , subscriptions = DevTools.toSubscriptions subscriptions
        , onUrlChange = DevTools.toMsg << onUrlChange
        , onUrlRequest = DevTools.toMsg << onUrlRequest
        }
