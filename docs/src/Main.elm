port module Main exposing (main)

import Browser
import Browser.DevTools
import Browser.Navigation as Navigation
import Element exposing (Element)
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
    , page : Page
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
init _ url key =
    ( { key = key
      , page = route url
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
            ( { model | page = route url }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Elm DevTools"
    , body =
        [ Element.layout []
            (Element.row []
                [ viewNavigation model.page
                , viewPage model.page
                ]
            )
        ]
    }


viewNavigation : Page -> Element Msg
viewNavigation page =
    Element.column []
        [ Element.link []
            { url = "#home"
            , label = Element.text "Elm DevTools"
            }
        , Element.link []
            { url = "#design"
            , label = Element.text "Design"
            }
        ]


viewPage : Page -> Element Msg
viewPage page =
    case page of
        Home ->
            viewHome

        Design ->
            viewDesign


viewHome : Element Msg
viewHome =
    Element.text "Home Page"


viewDesign : Element Msg
viewDesign =
    Element.text "Design Page"



-- Page


type Page
    = Home
    | Design


route : Url -> Page
route =
    Route.parse (Route.fragment pageFromFragment)
        >> Maybe.withDefault Home


pageFromFragment : Maybe String -> Page
pageFromFragment maybe =
    case maybe of
        Just "design" ->
            Design

        _ ->
            Home



-- Url


urlDecoder : Decoder Url
urlDecoder =
    Decode.andThen
        (Maybe.withDefault (Decode.fail "failed to parse Url")
            << Maybe.map Decode.succeed
            << Url.fromString
        )
        Decode.string


encodeUrl : Url -> Encode.Value
encodeUrl url =
    Encode.string (Url.toString url)



-- Model


encodeModel : Model -> Encode.Value
encodeModel _ =
    Encode.object
        [ ( "key", Encode.null )
        ]



-- Msg


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
