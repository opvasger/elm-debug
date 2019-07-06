port module Main exposing (main)

import Browser
import Browser.DevTools
import Browser.Navigation as Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Url exposing (Url)
import Url.Parser as Route


type alias Flags =
    { devTools : Maybe String
    }


type alias Model =
    { key : Navigation.Key
    }


type Msg
    = RequestUrl Browser.UrlRequest
    | ChangeUrl Url


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
init _ _ key =
    ( { key = key
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestUrl (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        RequestUrl (Browser.External url) ->
            ( model, Navigation.load url )

        ChangeUrl url ->
            ( model, Cmd.none )


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
                , viewDemo
                , viewFeatures
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


viewDemo : Element Msg
viewDemo =
    row
        [ width fill
        ]
        [ text "TODO Demo"
        ]


viewFeatures : Element Msg
viewFeatures =
    row
        [ width fill
        ]
        [ text "TODO Features" ]



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
encodeModel _ =
    Encode.object
        [ ( "key", Encode.null )
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


msgDecoder : Decoder Msg
msgDecoder =
    Decode.oneOf
        [ Decode.field "RequestInternalUrl" (Decode.map (RequestUrl << Browser.Internal) urlDecoder)
        , Decode.field "RequestExternalUrl" (Decode.map (RequestUrl << Browser.External) Decode.string)
        , Decode.field "ChangeUrl" (Decode.map ChangeUrl urlDecoder)
        ]
