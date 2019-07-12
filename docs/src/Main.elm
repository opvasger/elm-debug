port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import DevTools.Browser as DevTools
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Mario
import Page exposing (Page)
import Style
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


main : DevTools.Program Flags Model Msg
main =
    DevTools.application devToolFeatures
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = RequestUrl
        , onUrlChange = ChangeUrl
        }


devToolFeatures : List (DevTools.Feature Flags Model Msg)
devToolFeatures =
    [ DevTools.ViewMsgs encodeMsg
    , DevTools.ViewModel encodeModel
    , DevTools.ImportSession msgDecoder
    , DevTools.ExportSession encodeMsg
    , DevTools.CacheSession
        { encodeMsg = encodeMsg
        , msgDecoder = msgDecoder
        , fromCache = .devTools
        , toCache = toCache
        }
    ]


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags _ key =
    ( { key = key
      , mario = Mario.init flags.viewportWidth marioHeight
      , viewport = ( flags.viewportWidth, flags.viewportHeight )
      , page = Page.Splash
      }
    , Task.perform (\{ viewport } -> ResizeViewport (round viewport.width) (round viewport.height))
        Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize ResizeViewport
        , if model.page == Page.Mario then
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
            ( { model | page = Page.fromUrl url }
            , Cmd.none
            )

        ResizeViewport x y ->
            ( { model
                | viewport = ( x, y )
                , mario =
                    Mario.resize
                        { width = x
                        , height = model.mario.size.height
                        }
                        model.mario
              }
            , Cmd.none
            )

        MarioMsg marioMsg ->
            ( { model | mario = Mario.update marioMsg model.mario }
            , Cmd.none
            )


marioHeight : Int
marioHeight =
    205



-- View


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
                , map MarioMsg
                    (Page.view
                        { page = model.page
                        , mario = model.mario
                        , marioHeight = marioHeight
                        }
                    )
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
        , Background.color Style.lightBlue
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
                        , Font.color Style.white
                        ]
                        (text "Elm DevTools")
                    , el
                        [ centerX
                        , Font.color Style.darkBlue
                        ]
                        (text "Tools for developing Elm programs!")
                    ]
            }
        ]


viewNavigation : Page -> Element Msg
viewNavigation currentPage =
    row
        [ Font.size 18
        , centerX
        , spacing 30
        , paddingEach
            { top = 20
            , bottom = 0
            , left = 0
            , right = 0
            }
        ]
        [ viewNavigationLink Page.Start currentPage
        , viewNavigationLink Page.Design currentPage
        , viewNavigationLink Page.Goals currentPage
        , viewNavigationLink Page.Mario currentPage
        ]


viewNavigationLink : Page -> Page -> Element Msg
viewNavigationLink linkPage currentPage =
    let
        title =
            Page.title linkPage
    in
    if currentPage == linkPage then
        el [ centerX, Font.color Style.lightBlue ] (text title)

    else
        link [ centerX, Font.underline ]
            { url = Page.toUrl linkPage
            , label = text title
            }


viewFoot : Element Msg
viewFoot =
    column
        [ Background.color Style.darkBlue
        , Font.size 15
        , width fill
        ]
        [ row
            [ width fill
            , height (px 150)
            , Font.color Style.white
            , spacing 40
            ]
            [ viewFootColumn "Elm"
                [ ( "language website", "https://elm-lang.org" )
                , ( "evan's guide", "https://guide.elm-lang.org" )
                , ( "discourse", "https://discourse.elm-lang.org" )
                ]
            , viewFootColumn "DevTools"
                [ ( "node.js module", "https://www.npmjs.com/package/elm-devtools" )
                , ( "elm package", "https://package.elm-lang.org/packages/opvasger/devtools/latest/" )
                , ( "github", "https://github.com/opvasger/elm-devtools" )
                ]
            , el
                [ centerX
                , height fill
                , paddingXY 0 30
                ]
                (viewFootColumn "..."
                    [ ( "0.18 debugger", "https://elm-lang.org/blog/the-perfect-bug-report" )
                    , ( "bret's principle", "https://www.youtube.com/watch?v=PUv66718DII" )
                    ]
                )
            ]
        , viewCopyright
        ]


viewFootColumn : String -> List ( String, String ) -> Element Msg
viewFootColumn title links =
    column [ spacing 10, centerX ]
        (el [ Font.bold, Font.color Style.lightBlue ] (text title)
            :: List.map viewLink links
        )


viewLink : ( String, String ) -> Element Msg
viewLink ( title, url ) =
    newTabLink [ Font.underline ]
        { url = url
        , label = text title
        }


viewCopyright : Element Msg
viewCopyright =
    row
        [ Font.color Style.muted
        , padding 10
        , centerX
        ]
        [ text "This website is open-source "
        , viewLink ( "here", "https://github.com/opvasger/elm-devtools/tree/master/docs" )
        , text " © 2019, Asger Nielsen"
        ]



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
                    (Page.toString model.page)
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
