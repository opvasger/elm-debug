module DevTools.Browser exposing
    ( Feature(..)
    , Program
    , application
    , document
    , element
    , sandbox
    )

import Browser
import Browser.Navigation
import DevTools.Browser.Main as Main
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Url exposing (Url)


type alias Program flags model msg =
    Platform.Program flags (Main.Model model msg) (Main.Msg model msg)


sandbox :
    List (Feature flags model msg)
    ->
        { init : model
        , view : model -> Html msg
        , update : msg -> model -> model
        }
    -> Program flags model msg
sandbox features app =
    let
        config =
            List.foldl enableFeature noFeatures features
    in
    Browser.document
        { init =
            \flags ->
                Main.init
                    { update = \msg model -> ( app.update msg model, Cmd.none )
                    , msgDecoder = config.msgDecoder
                    , init = ( app.init, Cmd.none )
                    , fromCache = Maybe.andThen (\fn -> fn flags) config.fromCache
                    }
        , view =
            Main.view
                { encodeModel = config.encodeModel
                , encodeMsg = config.encodeMsg
                , view = \model -> { title = "", body = [ app.view model ] }
                , update = \msg model -> ( app.update msg model, Cmd.none )
                , isCacheEnabled = config.toCache /= Nothing
                , isImportEnabled = config.msgDecoder /= Nothing
                }
        , update =
            Main.update
                { msgDecoder = config.msgDecoder
                , encodeMsg = config.encodeMsg
                , toCache = config.toCache
                , update = \msg model -> ( app.update msg model, Cmd.none )
                }
        , subscriptions =
            Main.subscriptions
                { subscriptions = always Sub.none
                , msgDecoder = config.msgDecoder
                , update = \msg model -> ( app.update msg model, Cmd.none )
                }
        }


element :
    List (Feature flags model msg)
    ->
        { init : flags -> ( model, Cmd msg )
        , view : model -> Html msg
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        }
    -> Program flags model msg
element features app =
    let
        config =
            List.foldl enableFeature noFeatures features
    in
    Browser.element
        { init =
            \flags ->
                Main.init
                    { update = app.update
                    , msgDecoder = config.msgDecoder
                    , init = app.init flags
                    , fromCache = Maybe.andThen (\fn -> fn flags) config.fromCache
                    }
        , view =
            Main.view
                { encodeModel = config.encodeModel
                , encodeMsg = config.encodeMsg
                , view = app.view >> (\html -> { title = "", body = [ html ] })
                , update = app.update
                , isCacheEnabled = config.toCache /= Nothing
                , isImportEnabled = config.msgDecoder /= Nothing
                }
                >> .body
                >> List.head
                >> Maybe.withDefault (Html.text "")
        , update =
            Main.update
                { msgDecoder = config.msgDecoder
                , encodeMsg = config.encodeMsg
                , toCache = config.toCache
                , update = app.update
                }
        , subscriptions =
            Main.subscriptions
                { msgDecoder = config.msgDecoder
                , subscriptions = app.subscriptions
                , update = app.update
                }
        }


document :
    List (Feature flags model msg)
    ->
        { init : flags -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        }
    -> Program flags model msg
document features app =
    let
        config =
            List.foldl enableFeature noFeatures features
    in
    Browser.document
        { init =
            \flags ->
                Main.init
                    { update = app.update
                    , msgDecoder = config.msgDecoder
                    , init = app.init flags
                    , fromCache = Maybe.andThen (\fn -> fn flags) config.fromCache
                    }
        , view =
            Main.view
                { encodeModel = config.encodeModel
                , encodeMsg = config.encodeMsg
                , view = app.view
                , update = app.update
                , isCacheEnabled = config.toCache /= Nothing
                , isImportEnabled = config.msgDecoder /= Nothing
                }
        , update =
            Main.update
                { msgDecoder = config.msgDecoder
                , encodeMsg = config.encodeMsg
                , update = app.update
                , toCache = config.toCache
                }
        , subscriptions =
            Main.subscriptions
                { msgDecoder = config.msgDecoder
                , subscriptions = app.subscriptions
                , update = app.update
                }
        }


application :
    List (Feature flags model msg)
    ->
        { init : flags -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , onUrlChange : Url -> msg
        }
    -> Program flags model msg
application features app =
    let
        config =
            List.foldl enableFeature noFeatures features
    in
    Browser.application
        { init =
            \flags url key ->
                Main.init
                    { update = app.update
                    , msgDecoder = config.msgDecoder
                    , init = app.init flags url key
                    , fromCache = Maybe.andThen (\fn -> fn flags) config.fromCache
                    }
        , view =
            Main.view
                { encodeModel = config.encodeModel
                , encodeMsg = config.encodeMsg
                , view = app.view
                , update = app.update
                , isCacheEnabled = config.toCache /= Nothing
                , isImportEnabled = config.msgDecoder /= Nothing
                }
        , update =
            Main.update
                { msgDecoder = config.msgDecoder
                , encodeMsg = config.encodeMsg
                , update = app.update
                , toCache = config.toCache
                }
        , subscriptions =
            Main.subscriptions
                { msgDecoder = config.msgDecoder
                , subscriptions = app.subscriptions
                , update = app.update
                }
        , onUrlChange =
            app.onUrlChange >> Main.urlUpdate
        , onUrlRequest =
            app.onUrlRequest >> Main.urlUpdate
        }



-- Feature


type Feature flags model msg
    = ViewModel (model -> Encode.Value)
    | ViewMsgs (msg -> Encode.Value)
    | ExportSession (msg -> Encode.Value)
    | ImportSession (Decoder msg)
    | CacheSession
        { encodeMsg : msg -> Encode.Value
        , msgDecoder : Decoder msg
        , fromCache : flags -> Maybe String
        , toCache : String -> Cmd (Main.Msg model msg)
        }


type alias Config flags model msg =
    { encodeModel : Maybe (model -> Encode.Value)
    , encodeMsg : Maybe (msg -> Encode.Value)
    , msgDecoder : Maybe (Decoder msg)
    , fromCache : Maybe (flags -> Maybe String)
    , toCache : Maybe (String -> Cmd (Main.Msg model msg))
    }


noFeatures : Config flags model msg
noFeatures =
    { encodeModel = Nothing
    , encodeMsg = Nothing
    , msgDecoder = Nothing
    , fromCache = Nothing
    , toCache = Nothing
    }


enableFeature :
    Feature flags model msg
    -> Config flags model msg
    -> Config flags model msg
enableFeature feature config =
    case feature of
        ViewModel fn ->
            { config | encodeModel = Just fn }

        ViewMsgs fn ->
            { config | encodeMsg = Just fn }

        ImportSession fn ->
            { config | msgDecoder = Just fn }

        ExportSession fn ->
            { config | encodeMsg = Just fn }

        CacheSession { encodeMsg, msgDecoder, toCache, fromCache } ->
            { config
                | encodeMsg = Just encodeMsg
                , msgDecoder = Just msgDecoder
                , toCache = Just toCache
                , fromCache = Just fromCache
            }
