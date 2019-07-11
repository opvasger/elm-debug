module DevTools.Browser exposing
    ( Config(..)
    , Program
    , application
    , document
    , element
    , sandbox
    )

import Browser
import Browser.Navigation
import DevTools.Browser.Program as Program
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Url exposing (Url)


type alias Program flags model msg =
    Platform.Program flags (Program.Model model msg) (Program.Msg model msg)


sandbox :
    List (Config flags model msg)
    ->
        { init : model
        , view : model -> Html msg
        , update : msg -> model -> model
        }
    -> Program flags model msg
sandbox settings app =
    let
        config =
            List.foldl recordConfig noConfig settings
    in
    Browser.document
        { init =
            \flags ->
                Program.init
                    { update = \msg model -> ( app.update msg model, Cmd.none )
                    , msgDecoder = config.msgDecoder
                    , init = ( app.init, Cmd.none )
                    , fromCache = Maybe.andThen (\fn -> fn flags) config.fromCache
                    }
        , view =
            Program.view
                { encodeModel = config.encodeModel
                , encodeMsg = config.encodeMsg
                , view = \model -> { title = "", body = [ app.view model ] }
                , update = \msg model -> ( app.update msg model, Cmd.none )
                }
        , update =
            Program.update
                { msgDecoder = config.msgDecoder
                , encodeMsg = config.encodeMsg
                , toCache = config.toCache
                , update = \msg model -> ( app.update msg model, Cmd.none )
                }
        , subscriptions =
            Program.subscriptions
                { subscriptions = always Sub.none
                , msgDecoder = config.msgDecoder
                , update = \msg model -> ( app.update msg model, Cmd.none )
                }
        }


element :
    List (Config flags model msg)
    ->
        { init : flags -> ( model, Cmd msg )
        , view : model -> Html msg
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        }
    -> Program flags model msg
element settings app =
    let
        config =
            List.foldl recordConfig noConfig settings
    in
    Browser.element
        { init =
            \flags ->
                Program.init
                    { update = app.update
                    , msgDecoder = config.msgDecoder
                    , init = app.init flags
                    , fromCache = Maybe.andThen (\fn -> fn flags) config.fromCache
                    }
        , view =
            Program.view
                { encodeModel = config.encodeModel
                , encodeMsg = config.encodeMsg
                , view = app.view >> (\html -> { title = "", body = [ html ] })
                , update = app.update
                }
                >> .body
                >> List.head
                >> Maybe.withDefault (Html.text "")
        , update =
            Program.update
                { msgDecoder = config.msgDecoder
                , encodeMsg = config.encodeMsg
                , toCache = config.toCache
                , update = app.update
                }
        , subscriptions =
            Program.subscriptions
                { msgDecoder = config.msgDecoder
                , subscriptions = app.subscriptions
                , update = app.update
                }
        }


document :
    List (Config flags model msg)
    ->
        { init : flags -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        }
    -> Program flags model msg
document settings app =
    let
        config =
            List.foldl recordConfig noConfig settings
    in
    Browser.document
        { init =
            \flags ->
                Program.init
                    { update = app.update
                    , msgDecoder = config.msgDecoder
                    , init = app.init flags
                    , fromCache = Maybe.andThen (\fn -> fn flags) config.fromCache
                    }
        , view =
            Program.view
                { encodeModel = config.encodeModel
                , encodeMsg = config.encodeMsg
                , view = app.view
                , update = app.update
                }
        , update =
            Program.update
                { msgDecoder = config.msgDecoder
                , encodeMsg = config.encodeMsg
                , update = app.update
                , toCache = config.toCache
                }
        , subscriptions =
            Program.subscriptions
                { msgDecoder = config.msgDecoder
                , subscriptions = app.subscriptions
                , update = app.update
                }
        }


application :
    List (Config flags model msg)
    ->
        { init : flags -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , onUrlChange : Url -> msg
        }
    -> Program flags model msg
application settings app =
    let
        config =
            List.foldl recordConfig noConfig settings
    in
    Browser.application
        { init =
            \flags url key ->
                Program.init
                    { update = app.update
                    , msgDecoder = config.msgDecoder
                    , init = app.init flags url key
                    , fromCache = Maybe.andThen (\fn -> fn flags) config.fromCache
                    }
        , view =
            Program.view
                { encodeModel = config.encodeModel
                , encodeMsg = config.encodeMsg
                , view = app.view
                , update = app.update
                }
        , update =
            Program.update
                { msgDecoder = config.msgDecoder
                , encodeMsg = config.encodeMsg
                , update = app.update
                , toCache = config.toCache
                }
        , subscriptions =
            Program.subscriptions
                { msgDecoder = config.msgDecoder
                , subscriptions = app.subscriptions
                , update = app.update
                }
        , onUrlChange =
            app.onUrlChange >> Program.urlUpdate
        , onUrlRequest =
            app.onUrlRequest >> Program.urlUpdate
        }



-- Config


type Config flags model msg
    = ModelView (model -> Encode.Value)
    | MsgListView (msg -> Encode.Value)
    | BugReports
        { encodeMsg : msg -> Encode.Value
        , msgDecoder : Decoder msg
        }
    | AutoReplayMsgs
        { encodeMsg : msg -> Encode.Value
        , msgDecoder : Decoder msg
        , fromCache : flags -> Maybe String
        , toCache : String -> Cmd (Program.Msg model msg)
        }


type alias ConfigRecord flags model msg =
    { encodeModel : Maybe (model -> Encode.Value)
    , encodeMsg : Maybe (msg -> Encode.Value)
    , msgDecoder : Maybe (Decoder msg)
    , fromCache : Maybe (flags -> Maybe String)
    , toCache : Maybe (String -> Cmd (Program.Msg model msg))
    }


noConfig : ConfigRecord flags model msg
noConfig =
    { encodeModel = Nothing
    , encodeMsg = Nothing
    , msgDecoder = Nothing
    , fromCache = Nothing
    , toCache = Nothing
    }


recordConfig :
    Config flags model msg
    -> ConfigRecord flags model msg
    -> ConfigRecord flags model msg
recordConfig config record =
    case config of
        ModelView fn ->
            { record | encodeModel = Just fn }

        MsgListView fn ->
            { record | encodeMsg = Just fn }

        BugReports { encodeMsg, msgDecoder } ->
            { record
                | encodeMsg = Just encodeMsg
                , msgDecoder = Just msgDecoder
            }

        AutoReplayMsgs { encodeMsg, msgDecoder, toCache, fromCache } ->
            { record
                | encodeMsg = Just encodeMsg
                , msgDecoder = Just msgDecoder
                , toCache = Just toCache
                , fromCache = Just fromCache
            }
