port module Main exposing (main)

import Browser
import Browser.DevTools
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
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
    , viewport : ( Int, Int )
    , mario : Mario.Model
    , page : Page
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
      , mario = Mario.init flags.viewportWidth marioHeight
      , viewport = ( flags.viewportWidth, flags.viewportHeight )
      , page = Splash
      }
    , Task.perform (\{ viewport } -> ResizeViewport (round viewport.width) (round viewport.height))
        Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize ResizeViewport
        , if model.page == Mario then
            Sub.map MarioMsg (Mario.subscriptions model.mario)

          else
            Sub.none
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
            ( { model | page = pageFromUrl url }
            , Cmd.none
            )

        ResizeViewport x y ->
            ( { model | viewport = ( x, y ) }
            , Cmd.none
            )

        MarioMsg marioMsg ->
            ( { model | mario = Mario.update marioMsg model.mario }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Elm DevTools - Tools for developing Elm programs!"
    , body =
        [ layout
            [ Font.family
                [ Font.typeface "Helvetica"
                , Font.sansSerif
                ]
            ]
            (column [ width fill, height fill ]
                [ viewHead
                , viewNavigation model.page
                , el
                    [ height fill
                    , paddingXY 30 0
                    ]
                    (viewPage model)
                , viewFoot
                ]
            )
        ]
    }


viewHead : Element Msg
viewHead =
    row
        [ width fill
        , height (px 125)
        , Background.color lightBlue
        ]
        [ link
            [ width fill ]
            { url = "/"
            , label =
                column [ width fill, spacing 5 ]
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
            }
        ]


viewNavigation : Page -> Element Msg
viewNavigation page =
    row
        [ width fill
        , Font.size 18
        , spacing 30
        , padding 20
        ]
        [ if page == Start then
            el [ centerX, Font.color lightBlue ] (text "get started")

          else
            link [ centerX, Font.underline ]
                { url = "#start"
                , label = text "get started"
                }
        , if page == Features then
            el [ centerX, Font.color lightBlue ] (text "features")

          else
            link [ centerX, Font.underline ]
                { url = "#features"
                , label = text "features"
                }
        , if page == Goals then
            el [ centerX, Font.color lightBlue ] (text "goals")

          else
            link [ centerX, Font.underline ]
                { url = "#goals"
                , label = text "goals"
                }
        , if page == Mario then
            el [ centerX, Font.color lightBlue ] (text "mario")

          else
            link [ centerX, Font.underline ]
                { url = "#mario"
                , label = text "mario"
                }
        ]


viewPage : Model -> Element Msg
viewPage model =
    case model.page of
        Splash ->
            viewSplash

        Start ->
            viewStart

        Features ->
            viewFeatures

        Goals ->
            viewGoals

        Mario ->
            Mario.view model.mario
                |> html
                |> map MarioMsg


marioHeight : Int
marioHeight =
    425


viewSplash : Element Msg
viewSplash =
    none


viewStart : Element Msg
viewStart =
    none


viewFeatures : Element Msg
viewFeatures =
    none


viewGoals : Element Msg
viewGoals =
    none


viewFoot : Element Msg
viewFoot =
    column
        [ Background.color darkBlue
        , Font.size 15
        , width fill
        ]
        [ row
            [ width fill
            , height (px 150)
            , Font.color white
            , spacing 40
            ]
            [ column [ spacing 10, centerX ]
                [ el [ Font.bold, Font.color lightBlue ] (text "This Project")
                , newTabLink [ Font.underline ]
                    { url = "https://github.com/opvasger/elm-devtools"
                    , label = text "github repository"
                    }
                , newTabLink [ Font.underline ]
                    { url = "https://www.npmjs.com/package/elm-devtools"
                    , label = text "node.js module"
                    }
                , newTabLink [ Font.underline ]
                    { url = "https://package.elm-lang.org/packages/opvasger/devtools/latest/"
                    , label = text "elm package"
                    }
                ]
            , column [ spacing 10, centerX ]
                [ el [ Font.bold, Font.color lightBlue ] (text "Elm")
                , newTabLink [ Font.underline ]
                    { url = "https://elm-lang.org"
                    , label = text "language website"
                    }
                , newTabLink [ Font.underline ]
                    { url = "https://guide.elm-lang.org"
                    , label = text "official guide"
                    }
                , newTabLink [ Font.underline ]
                    { url = "https://discourse.elm-lang.org"
                    , label = text "discourse"
                    }
                ]
            , column [ spacing 10, centerX ]
                [ el [ Font.bold, Font.color lightBlue ] (text "Inspired By")
                , newTabLink [ Font.underline ]
                    { url = "https://elm-lang.org/blog/the-perfect-bug-report"
                    , label = text "0.18 debugger"
                    }
                , newTabLink [ Font.underline ]
                    { url = "https://www.youtube.com/watch?v=PUv66718DII"
                    , label = text "bret's principle"
                    }
                , el [ height (px 15) ] none
                ]
            ]
        , row
            [ Font.color (rgba255 0 0 0 1)
            , padding 10
            , centerX
            ]
            [ text "This website is open-source "
            , newTabLink [ Font.underline ]
                { url = "https://github.com/opvasger/elm-devtools/tree/master/docs"
                , label = text "here"
                }
            , text " © 2019, Asger Nielsen"
            ]
        ]



-- Page


type Page
    = Splash
    | Start
    | Features
    | Goals
    | Mario


pageFromUrl : Url -> Page
pageFromUrl url =
    case url.fragment of
        Just "start" ->
            Start

        Just "features" ->
            Features

        Just "goals" ->
            Goals

        Just "mario" ->
            Mario

        _ ->
            Splash


pageToString : Page -> Maybe String
pageToString page =
    case page of
        Start ->
            Just "start"

        Features ->
            Just "features"

        Goals ->
            Just "goals"

        Mario ->
            Just "mario"

        Splash ->
            Nothing



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
        , ( "page"
          , Maybe.withDefault Encode.null
                (Maybe.map Encode.string
                    (pageToString model.page)
                )
          )
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
