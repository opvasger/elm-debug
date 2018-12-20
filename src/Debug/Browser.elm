module Debug.Browser exposing (application, document, element, sandbox)

import Browser
import Browser.Navigation
import Debug.Browser.Main as Main
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Url exposing (Url)


sandbox :
    { init : model
    , view : model -> Html msg
    , update : msg -> model -> model
    , debug : Main.Configuration model msg
    }
    -> Main.Program model msg
sandbox { init, view, update, debug } =
    Browser.document
        { init =
            \flags ->
                Main.wrapInit
                    { update = \msg model -> ( update msg model, Cmd.none )
                    , msgDecoder = debug.msgDecoder
                    , flags = flags
                    , model = init
                    , cmds = Cmd.none
                    }
        , view =
            Main.wrapDocument
                { printModel = debug.printModel
                , encodeMsg = debug.encodeMsg
                , view = \model -> { title = "", body = view model :: [] }
                }
        , update =
            Main.wrapUpdate
                { msgDecoder = debug.msgDecoder
                , encodeMsg = debug.encodeMsg
                , toPort = debug.toPort
                , update = \msg model -> ( update msg model, Cmd.none )
                }
        , subscriptions =
            Main.wrapSubscriptions
                { subscriptions = always Sub.none
                , msgDecoder = debug.msgDecoder
                }
        }


element :
    { init : Json.Decode.Value -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , debug : Main.Configuration model msg
    }
    -> Main.Program model msg
element { init, view, update, subscriptions, debug } =
    Browser.element
        { init =
            \flags ->
                let
                    ( model, cmds ) =
                        init flags
                in
                Main.wrapInit
                    { update = update
                    , msgDecoder = debug.msgDecoder
                    , flags = flags
                    , model = model
                    , cmds = cmds
                    }
        , view =
            Main.wrapHtml
                { printModel = debug.printModel
                , encodeMsg = debug.encodeMsg
                , view = view
                }
        , update =
            Main.wrapUpdate
                { msgDecoder = debug.msgDecoder
                , encodeMsg = debug.encodeMsg
                , toPort = debug.toPort
                , update = update
                }
        , subscriptions =
            Main.wrapSubscriptions
                { msgDecoder = debug.msgDecoder
                , subscriptions = subscriptions
                }
        }


document :
    { init : Json.Decode.Value -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , debug : Main.Configuration model msg
    }
    -> Main.Program model msg
document { init, view, update, subscriptions, debug } =
    Browser.document
        { init =
            \flags ->
                let
                    ( model, cmds ) =
                        init flags
                in
                Main.wrapInit
                    { update = update
                    , msgDecoder = debug.msgDecoder
                    , flags = flags
                    , model = model
                    , cmds = cmds
                    }
        , view =
            Main.wrapDocument
                { printModel = debug.printModel
                , encodeMsg = debug.encodeMsg
                , view = view
                }
        , update =
            Main.wrapUpdate
                { msgDecoder = debug.msgDecoder
                , encodeMsg = debug.encodeMsg
                , update = update
                , toPort = debug.toPort
                }
        , subscriptions =
            Main.wrapSubscriptions
                { subscriptions = subscriptions
                , msgDecoder = debug.msgDecoder
                }
        }


application :
    { init : Json.Decode.Value -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url -> msg
    , debug : Main.Configuration model msg
    }
    -> Main.Program model msg
application { init, view, update, subscriptions, onUrlRequest, onUrlChange, debug } =
    Browser.application
        { init =
            \flags url key ->
                let
                    ( model, cmds ) =
                        init flags url key
                in
                Main.wrapInit
                    { update = update
                    , msgDecoder = debug.msgDecoder
                    , flags = flags
                    , model = model
                    , cmds = cmds
                    }
        , view =
            Main.wrapDocument
                { printModel = debug.printModel
                , encodeMsg = debug.encodeMsg
                , view = view
                }
        , update =
            Main.wrapUpdate
                { msgDecoder = debug.msgDecoder
                , encodeMsg = debug.encodeMsg
                , update = update
                , toPort = debug.toPort
                }
        , subscriptions =
            Main.wrapSubscriptions
                { subscriptions = subscriptions
                , msgDecoder = debug.msgDecoder
                }
        , onUrlChange = Main.wrapMsg onUrlChange
        , onUrlRequest = Main.wrapMsg onUrlRequest
        }
