module Form exposing
    ( encodeMsg
    , fromCache
    , init
    , msgDecoder
    , subscriptions
    , update
    , view
    )

import Browser
import Html as H exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import Json.Decode as Jd
import Json.Encode as Je


type alias Flags =
    { devTools : Maybe String
    }


fromCache : Flags -> Maybe String
fromCache =
    .devTools


type Model
    = InAuth AuthModel
    | InAccount AccountModel


type Msg
    = FromAuth AuthMsg
    | FromAccount AccountMsg


msgDecoder : Jd.Decoder Msg
msgDecoder =
    Jd.oneOf
        [ Jd.field "Log In" (Jd.null (FromAuth LogIn))
        , Jd.field "Log Out" (Jd.null (FromAccount LogOut))
        , Jd.map (FromAuth << InputName) (Jd.field "Input Name" Jd.string)
        , Jd.map (FromAuth << InputPass) (Jd.field "Input Pass" Jd.string)
        , Jd.map (FromAccount << InputNotes) (Jd.field "Input Notes" Jd.string)
        ]


encodeMsg : Msg -> Je.Value
encodeMsg msg =
    case msg of
        FromAuth (InputName name) ->
            Je.object [ ( "Input Name", Je.string name ) ]

        FromAuth (InputPass pass) ->
            Je.object [ ( "Input Pass", Je.string pass ) ]

        FromAuth LogIn ->
            Je.object [ ( "Log In", Je.null ) ]

        FromAccount LogOut ->
            Je.object [ ( "Log Out", Je.null ) ]

        FromAccount (InputNotes notes) ->
            Je.object [ ( "Input Notes", Je.string notes ) ]


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( InAuth initAuth
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( FromAuth authMsg, InAuth authModel ) ->
            let
                updated =
                    updateAuth authMsg authModel
            in
            case ( authMsg, updated.status ) of
                ( LogIn, LoggingIn ) ->
                    ( InAccount (initAccount updated.name)
                    , Cmd.none
                    )

                _ ->
                    ( InAuth updated
                    , Cmd.none
                    )

        ( FromAccount acntMsg, InAccount acntModel ) ->
            let
                updated =
                    updateAccount acntMsg acntModel
            in
            case ( acntMsg, updated.status ) of
                ( LogOut, LoggingOut ) ->
                    ( InAuth initAuth
                    , Cmd.none
                    )

                _ ->
                    ( InAccount updated
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Example"
    , body =
        case model of
            InAuth authModel ->
                [ H.map FromAuth (viewAuth authModel) ]

            InAccount acntModel ->
                [ H.map FromAccount (viewAccount acntModel) ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Auth


type alias AuthModel =
    { name : String
    , pass : String
    , status : AuthStatus
    }


type AuthStatus
    = AuthReady
    | MissingName
    | MissingPass
    | LoggingIn


printAuthStatus : AuthStatus -> String
printAuthStatus status =
    case status of
        AuthReady ->
            ""

        MissingName ->
            "Please specify your name"

        MissingPass ->
            "Please specify your password"

        LoggingIn ->
            "..."


type AuthMsg
    = InputName String
    | InputPass String
    | LogIn


initAuth : AuthModel
initAuth =
    { name = ""
    , pass = ""
    , status = AuthReady
    }


updateAuth : AuthMsg -> AuthModel -> AuthModel
updateAuth msg model =
    case msg of
        InputName name ->
            { model | name = name, status = AuthReady }

        InputPass pass ->
            { model | pass = pass, status = AuthReady }

        LogIn ->
            { model | status = authenticate model }


authenticate : AuthModel -> AuthStatus
authenticate model =
    if String.length (String.trim model.name) < 1 then
        MissingName

    else if String.length (String.trim model.pass) < 1 then
        MissingPass

    else
        LoggingIn


viewAuth : AuthModel -> Html AuthMsg
viewAuth model =
    H.div
        centerAttributes
        [ H.input
            [ Ha.type_ "text"
            , Ha.placeholder "name"
            , He.onInput InputName
            , Ha.value model.name
            ]
            []
        , H.input
            [ Ha.type_ "password"
            , Ha.placeholder "pass"
            , He.onInput InputPass
            , Ha.value model.pass
            ]
            []
        , H.button
            [ He.onClick LogIn
            ]
            [ H.text "Log In"
            ]
        , H.text (printAuthStatus model.status)
        ]



-- Account


type alias AccountModel =
    { name : String
    , status : AccountStatus
    , notes : String
    }


type AccountStatus
    = AccountReady
    | LoggingOut


type AccountMsg
    = LogOut
    | InputNotes String


initAccount : String -> AccountModel
initAccount name =
    { name = name
    , status = AccountReady
    , notes = ""
    }


updateAccount : AccountMsg -> AccountModel -> AccountModel
updateAccount msg model =
    case msg of
        LogOut ->
            { model | status = LoggingOut }

        InputNotes notes ->
            { model | notes = notes }


viewAccount : AccountModel -> Html AccountMsg
viewAccount model =
    H.div centerAttributes
        [ H.text ("Hello there, " ++ model.name)
        , H.button
            [ He.onClick LogOut
            ]
            [ H.text "Log Out"
            ]
        , H.text "How are you feeling?"
        , H.textarea
            [ Ha.value model.notes
            , He.onInput InputNotes
            ]
            []
        ]



-- Helpers


centerAttributes : List (H.Attribute msg)
centerAttributes =
    [ Ha.style "height" "95vh"
    , Ha.style "display" "flex"
    , Ha.style "flex-direction" "column"
    , Ha.style "align-items" "center"
    , Ha.style "justify-content" "center"
    ]
