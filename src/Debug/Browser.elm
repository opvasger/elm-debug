module Debug.Browser exposing (Program, application, document, element, sandbox)

import Browser
import Browser.Navigation
import Debug.Browser.Main as Bdm
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Url exposing (Url)


type alias Program model msg =
    Bdm.Program model msg


sandbox :
    { init : model
    , view : model -> Html msg
    , update : msg -> model -> model
    , debug : Bdm.Configuration model msg
    }
    -> Bdm.Program model msg
sandbox { init, view, update, debug } =
    Browser.document
        { init =
            \flags ->
                Bdm.wrapInit
                    { update = \msg model -> ( update msg model, Cmd.none )
                    , msgDecoder = debug.msgDecoder
                    , flags = flags
                    , model = init
                    , cmds = Cmd.none
                    }
        , view =
            Bdm.wrapDocument
                { printModel = debug.printModel
                , encodeMsg = debug.encodeMsg
                , view = \model -> { title = "", body = view model :: [] }
                }
        , update =
            Bdm.wrapUpdate
                { msgDecoder = debug.msgDecoder
                , encodeMsg = debug.encodeMsg
                , toPort = debug.toPort
                , update = \msg model -> ( update msg model, Cmd.none )
                }
        , subscriptions =
            Bdm.wrapSubscriptions
                { subscriptions = always Sub.none
                , msgDecoder = debug.msgDecoder
                }
        }


element :
    { init : Json.Decode.Value -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , debug : Bdm.Configuration model msg
    }
    -> Bdm.Program model msg
element { init, view, update, subscriptions, debug } =
    Browser.element
        { init =
            \flags ->
                let
                    ( model, cmds ) =
                        init flags
                in
                Bdm.wrapInit
                    { update = update
                    , msgDecoder = debug.msgDecoder
                    , flags = flags
                    , model = model
                    , cmds = cmds
                    }
        , view =
            Bdm.wrapHtml
                { printModel = debug.printModel
                , encodeMsg = debug.encodeMsg
                , view = view
                }
        , update =
            Bdm.wrapUpdate
                { msgDecoder = debug.msgDecoder
                , encodeMsg = debug.encodeMsg
                , toPort = debug.toPort
                , update = update
                }
        , subscriptions =
            Bdm.wrapSubscriptions
                { msgDecoder = debug.msgDecoder
                , subscriptions = subscriptions
                }
        }


document :
    { init : Json.Decode.Value -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , debug : Bdm.Configuration model msg
    }
    -> Bdm.Program model msg
document { init, view, update, subscriptions, debug } =
    Browser.document
        { init =
            \flags ->
                let
                    ( model, cmds ) =
                        init flags
                in
                Bdm.wrapInit
                    { update = update
                    , msgDecoder = debug.msgDecoder
                    , flags = flags
                    , model = model
                    , cmds = cmds
                    }
        , view =
            Bdm.wrapDocument
                { printModel = debug.printModel
                , encodeMsg = debug.encodeMsg
                , view = view
                }
        , update =
            Bdm.wrapUpdate
                { msgDecoder = debug.msgDecoder
                , encodeMsg = debug.encodeMsg
                , update = update
                , toPort = debug.toPort
                }
        , subscriptions =
            Bdm.wrapSubscriptions
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
    , debug : Bdm.Configuration model msg
    }
    -> Bdm.Program model msg
application { init, view, update, subscriptions, onUrlRequest, onUrlChange, debug } =
    Browser.application
        { init =
            \flags url key ->
                let
                    ( model, cmds ) =
                        init flags url key
                in
                Bdm.wrapInit
                    { update = update
                    , msgDecoder = debug.msgDecoder
                    , flags = flags
                    , model = model
                    , cmds = cmds
                    }
        , view =
            Bdm.wrapDocument
                { printModel = debug.printModel
                , encodeMsg = debug.encodeMsg
                , view = view
                }
        , update =
            Bdm.wrapUpdate
                { msgDecoder = debug.msgDecoder
                , encodeMsg = debug.encodeMsg
                , update = update
                , toPort = debug.toPort
                }
        , subscriptions =
            Bdm.wrapSubscriptions
                { subscriptions = subscriptions
                , msgDecoder = debug.msgDecoder
                }
        , onUrlChange = Bdm.wrapMsg onUrlChange
        , onUrlRequest = Bdm.wrapMsg onUrlRequest
        }
