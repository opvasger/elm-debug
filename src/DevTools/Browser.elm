module DevTools.Browser exposing
    ( Program, sandbox, element, document, application
    , Feature(..)
    )

{-| This module helps you develop Elm programs by providing you with additional information and tooling. This is presented at runtime as a small UI-element.


# Programs

@docs Program, sandbox, element, document, application


# Features

This API is incremental, and comes with some default features that doesn't require any user-defined functions:

1.  Restart your application by resetting the state and performing initial commands.
2.  Navigate previous states of your application fast and easily. The application is paused during this navigation, but can be resumed from any of those states. Be aware that messages produced by the initial command CANNOT be removed from the history, and will persist unless until you restart it.

@docs Feature

-}

import Browser
import Browser.Navigation
import DevTools.Browser.Main as Main
import Html exposing (Html)
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import Url exposing (Url)


{-| These program-constructors are generally meant for development purposes. The API mirror [`elm/browser`](https://package.elm-lang.org/packages/elm/browser/latest/Browser), so you can easily toggle your program between development and production.
-}
type alias Program flags model msg =
    Platform.Program flags (Main.Model model msg) (Main.Msg model msg)


{-| Create a “sandboxed” program that cannot communicate with the outside world. More about that [here](https://package.elm-lang.org/packages/elm/browser/latest/Browser#sandbox).
-}
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
            List.foldl unlockFeature noFeatures features
    in
    Browser.document
        { init =
            \flags ->
                Main.init
                    { update = \msg model -> ( app.update msg model, Cmd.none )
                    , msgDecoder = config.msgDecoder
                    , init = ( app.init, Cmd.none )
                    , fromCache = Maybe.andThen (\fn -> fn flags) config.fromCache
                    , isExportEnabled = config.encodeMsg /= Nothing
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


{-| Create an HTML element managed by Elm. More about that [here](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element).
-}
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
            List.foldl unlockFeature noFeatures features
    in
    Browser.element
        { init =
            \flags ->
                Main.init
                    { update = app.update
                    , msgDecoder = config.msgDecoder
                    , init = app.init flags
                    , fromCache = Maybe.andThen (\fn -> fn flags) config.fromCache
                    , isExportEnabled = config.encodeMsg /= Nothing
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


{-| Create an HTML document managed by Elm. More about that [here](https://package.elm-lang.org/packages/elm/browser/latest/Browser#document).
-}
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
            List.foldl unlockFeature noFeatures features
    in
    Browser.document
        { init =
            \flags ->
                Main.init
                    { update = app.update
                    , msgDecoder = config.msgDecoder
                    , init = app.init flags
                    , fromCache = Maybe.andThen (\fn -> fn flags) config.fromCache
                    , isExportEnabled = config.encodeMsg /= Nothing
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


{-| Create an application that manages Url changes. More about that [here](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application).
-}
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
            List.foldl unlockFeature noFeatures features
    in
    Browser.application
        { init =
            \flags url key ->
                Main.init
                    { update = app.update
                    , msgDecoder = config.msgDecoder
                    , init = app.init flags url key
                    , fromCache = Maybe.andThen (\fn -> fn flags) config.fromCache
                    , isExportEnabled = config.encodeMsg /= Nothing
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


{-| You can unlock additional features that can aid you during development, depending on what you need:


### ViewModel

Sometimes it can be useful to inspect the state of your model over time. By providing a JSON-encoder for the model of your application, you can inspect it in real-time, and easily understand what values you're dealing with and how they changed over time.


### ViewMsgs

Messages drive interaction in Elm applications due to [The Elm Architecture](https://guide.elm-lang.org/architecture). By providing a JSON-encoder for messages in your application, you unlock a list of all interactions. This can give you more insight into what interactions were produced, which order they appeared in and where they came from.


### ExportSession

A session contains all interactions with your application + devtools during development/debugging. This state can be exported to a JSON-file, which can be really useful for, for example, bug-reports. If you have a QA-team, they would probobably appreciate receiving a bug-report where they can reliably reproduce the error you encountered, along with a description and relevant date-information. This is all built into this feature.

I suggest attaching a git-hash to this message to ensure they're running the same code as you.


### ImportSession

This is the other half of `ExportSession`, which will have to be defined if anyone will want to load the bug-report you produced. These two features are separate to facilitate producing bug-reports without having an up-front way to load them.


### CacheSession

If devtools is provided a way to persist messages between browser-reloads, you can essentially maintain state during development. The really cool thing about this is that because of the way Elm (and its architecture) works, the only way that you can break your application-state is modifying/removing constructors for your message-type. This is VERY predictable because modifying these constructors is the essence of rendering them impossible in the first place:

  - If `LogIn` is modified/removed, does it even make any sense to `ViewAccount`? Probably not.
  - Are your application-messages as coupled as `LogIn` and `ViewAccount` obviously would be? If not, you have to option to modify how this feature works. You get to choose between:
    1.  Read from the cache until the first incompatible message. If `LogIn` is modified, `ViewAccount` will never happen.
    2.  Read from the cache and skip incompatible messages. This can work unpredictably if messages are coupled tightly, but works great if they're not.
    3.  Read from the cache if all messages are compatible. This is how the `ImportSession`-feature works.

-}
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


unlockFeature :
    Feature flags model msg
    -> Config flags model msg
    -> Config flags model msg
unlockFeature feature config =
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
