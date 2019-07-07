port module Main exposing (main)

import Browser
import Browser.DevTools
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Mario
import Task
import Url exposing (Url)


type alias Flags =
    { devTools : Maybe String
    , viewportHeight : Int
    , viewportWidth : Int
    }


type alias Model =
    { key : Navigation.Key
    , mario : Mario.Model
    , viewport : ( Int, Int )
    }


type Msg
    = RequestUrl Browser.UrlRequest
    | ChangeUrl Url
    | ResizeViewport Int Int
    | MarioMsg Mario.Msg


port toCache : String -> Cmd msg


main : Browser.DevTools.Program Flags Model Msg
main =
    Browser.DevTools.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = RequestUrl
        , onUrlChange = ChangeUrl
        , devTools =
            { encodeModel = encodeModel
            , encodeMsg = encodeMsg
            , msgDecoder = msgDecoder
            , fromCache = .devTools
            , toCache = toCache
            }
        }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags _ key =
    ( { key = key
      , mario = Mario.init flags.viewportWidth demoHeight
      , viewport = ( flags.viewportWidth, flags.viewportHeight )
      }
    , Task.perform (\{ viewport } -> ResizeViewport (round viewport.width) (round viewport.height))
        Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize ResizeViewport
        , Sub.map MarioMsg (Mario.subscriptions model.mario)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestUrl (Browser.Internal url) ->
            ( model
            , Navigation.pushUrl model.key
                (Url.toString url)
            )

        RequestUrl (Browser.External url) ->
            ( model
            , Navigation.load url
            )

        ChangeUrl url ->
            ( model
            , Cmd.none
            )

        ResizeViewport x y ->
            ( { model
                | viewport = ( x, y )
                , mario = Mario.resize { width = x, height = model.mario.size.height } model.mario
              }
            , Cmd.none
            )

        MarioMsg marioMsg ->
            ( { model | mario = Mario.update marioMsg model.mario }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Elm DevTools"
    , body =
        [ layout
            [ Font.family
                [ Font.typeface "Helvetica"
                , Font.sansSerif
                ]
            ]
            (column [ width fill ]
                [ viewHead
                , viewDemo model.mario
                , viewFeatures
                , viewLinks
                ]
            )
        ]
    }


viewHead : Element Msg
viewHead =
    row
        [ width fill
        , height (px 150)
        , Background.color lightBlue
        ]
        [ column [ width fill, spacing 10 ]
            [ el
                [ centerX
                , Font.bold
                , Font.size 50
                , Font.color white
                ]
                (text "Elm DevTools")
            , el
                [ centerX
                , Font.color darkBlue
                ]
                (text "Tools for developing Elm programs!")
            ]
        ]


viewDemo : Mario.Model -> Element Msg
viewDemo mario =
    row
        [ width fill
        , height (px demoHeight)
        , Background.color white
        ]
        [ Element.map MarioMsg (html (Mario.view mario))
        ]


demoHeight : Int
demoHeight =
    200


viewFeatures : Element Msg
viewFeatures =
    column
        [ width fill
        ]
        [ text "Features"
        , column [ padding 10 ]
            [ text "Automatic message-replay"
            , text "Predictable live-coding"
            , text "Reasonable performance"
            , text "State à la carte"
            ]
        ]


viewLinks : Element Msg
viewLinks =
    column
        [ width fill
        ]
        [ text "Links"
        , column [ padding 10, Font.underline ]
            [ newTabLink []
                { label = text "Github Repo"
                , url = "https://github.com/opvasger/elm-devtools"
                }
            , newTabLink []
                { label = text "Elm Package"
                , url = "https://package.elm-lang.org/packages/opvasger/devtools/latest"
                }
            , newTabLink []
                { label = text "NPM Package"
                , url = "https://www.npmjs.com/package/elm-devtools"
                }
            , newTabLink []
                { label = text "Official Elm Guide"
                , url = "https://guide.elm-lang.org"
                }
            , newTabLink []
                { label = text "Official Elm Website"
                , url = "https://elm-lang.org"
                }
            ]
        ]



-- Colors


lightBlue : Color
lightBlue =
    rgba255 96 181 204 1


darkBlue : Color
darkBlue =
    rgba255 52 73 94 1


white : Color
white =
    rgba255 255 255 255 1



-- Json


encodeUrl : Url -> Encode.Value
encodeUrl =
    Encode.string << Url.toString


urlDecoder : Decoder Url
urlDecoder =
    Decode.andThen
        (Maybe.withDefault (Decode.fail "invalid URL")
            << Maybe.map Decode.succeed
            << Url.fromString
        )
        Decode.string


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "key", Encode.null )
        , ( "mario", Mario.encodeModel model.mario )
        ]


encodeMsg : Msg -> Encode.Value
encodeMsg msg =
    case msg of
        RequestUrl (Browser.Internal url) ->
            Encode.object [ ( "RequestInternalUrl", encodeUrl url ) ]

        RequestUrl (Browser.External url) ->
            Encode.object [ ( "RequestExternalUrl", Encode.string url ) ]

        ChangeUrl url ->
            Encode.object [ ( "ChangeUrl", Encode.string (Url.toString url) ) ]

        ResizeViewport x y ->
            Encode.object [ ( "x", Encode.int x ), ( "y", Encode.int y ) ]

        MarioMsg marioMsg ->
            Mario.encodeMsg marioMsg


msgDecoder : Decoder Msg
msgDecoder =
    Decode.oneOf
        [ Decode.field "RequestInternalUrl" (Decode.map (RequestUrl << Browser.Internal) urlDecoder)
        , Decode.field "RequestExternalUrl" (Decode.map (RequestUrl << Browser.External) Decode.string)
        , Decode.field "ChangeUrl" (Decode.map ChangeUrl urlDecoder)
        , Decode.map2 ResizeViewport (Decode.field "x" Decode.int) (Decode.field "y" Decode.int)
        , Decode.map MarioMsg Mario.msgDecoder
        ]
