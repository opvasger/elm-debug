module DevTools.Browser exposing (application, document, element, sandbox)

import Browser
import Browser.Navigation
import DevTools.Browser.Main as Main
import Html exposing (Html)
import Url exposing (Url)


sandbox :
    { init : model
    , view : model -> Html msg
    , update : msg -> model -> model
    , devTools : Main.Configuration flags model msg
    }
    -> Main.Program flags model msg
sandbox { init, view, update, devTools } =
    Browser.document
        { init =
            \flags ->
                Main.toInit
                    { update = \msg model -> ( update msg model, Cmd.none )
                    , msgDecoder = devTools.msgDecoder
                    , modelCmdPair = ( init, Cmd.none )
                    , session = devTools.toSession flags
                    }
        , view =
            Main.toDocument
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , view = \model -> { title = "", body = view model :: [] }
                }
        , update =
            Main.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , output = devTools.output
                , update = \msg model -> ( update msg model, Cmd.none )
                }
        , subscriptions =
            Main.toSubscriptions
                { subscriptions = always Sub.none
                , msgDecoder = devTools.msgDecoder
                }
        }


element :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , devTools : Main.Configuration flags model msg
    }
    -> Main.Program flags model msg
element { init, view, update, subscriptions, devTools } =
    Browser.element
        { init =
            \flags ->
                Main.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , modelCmdPair = init flags
                    , session = devTools.toSession flags
                    }
        , view =
            Main.toHtml
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , view = view
                }
        , update =
            Main.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , output = devTools.output
                , update = update
                }
        , subscriptions =
            Main.toSubscriptions
                { msgDecoder = devTools.msgDecoder
                , subscriptions = subscriptions
                }
        }


document :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , devTools : Main.Configuration flags model msg
    }
    -> Main.Program flags model msg
document { init, view, update, subscriptions, devTools } =
    Browser.document
        { init =
            \flags ->
                Main.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , modelCmdPair = init flags
                    , session = devTools.toSession flags
                    }
        , view =
            Main.toDocument
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , view = view
                }
        , update =
            Main.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , update = update
                , output = devTools.output
                }
        , subscriptions =
            Main.toSubscriptions
                { msgDecoder = devTools.msgDecoder
                , subscriptions = subscriptions
                }
        }


application :
    { init : flags -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url -> msg
    , devTools : Main.Configuration flags model msg
    }
    -> Main.Program flags model msg
application { init, view, update, subscriptions, onUrlRequest, onUrlChange, devTools } =
    Browser.application
        { init =
            \flags url key ->
                Main.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , modelCmdPair = init flags url key
                    , session = devTools.toSession flags
                    }
        , view =
            Main.toDocument
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , view = view
                }
        , update =
            Main.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , update = update
                , output = devTools.output
                }
        , subscriptions =
            Main.toSubscriptions
                { msgDecoder = devTools.msgDecoder
                , subscriptions = subscriptions
                }
        , onUrlChange = Main.toMsg << onUrlChange
        , onUrlRequest = Main.toMsg << onUrlRequest
        }
