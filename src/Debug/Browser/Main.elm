module Debug.Browser.Main exposing
    ( Configuration
    , Program
    , wrapDocument
    , wrapHtml
    , wrapInit
    , wrapMsg
    , wrapSubscriptions
    , wrapUpdate
    )

import Browser
import Browser.Dom as Dom
import Debug.Browser.History as History exposing (History)
import Debug.Browser.View as View
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Position exposing (Position)
import Size exposing (Size)
import Task


type alias Configuration model msg =
    { printModel : model -> String
    , encodeMsg : msg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder msg
    , toPort : Json.Encode.Value -> Cmd (Msg msg)
    }


type alias Program model msg =
    Platform.Program Json.Decode.Value (Model model msg) (Msg msg)


type alias Model model msg =
    { history : History model msg
    , position : Position
    , viewportSize : Size
    , isModelOverlayed : Bool
    }


type Msg msg
    = Update msg
    | ResizeViewport Size


mapUpdate : Cmd msg -> Cmd (Msg msg)
mapUpdate =
    Cmd.map Update


wrapMsg : (a -> msg) -> a -> Msg msg
wrapMsg toMsg =
    Update << toMsg


wrapInit :
    { model : model
    , cmds : Cmd msg
    , flags : Json.Decode.Value
    , msgDecoder : Json.Decode.Decoder msg
    , update : msg -> model -> ( model, Cmd msg )
    }
    -> ( Model model msg, Cmd (Msg msg) )
wrapInit config =
    ( { history = History.init config.model
      , position = Position 0 0
      , viewportSize = Size 0 0
      , isModelOverlayed = True
      }
    , Cmd.batch
        [ mapUpdate config.cmds
        , Task.perform ResizeViewport Size.getViewportSize
        ]
    )


wrapSubscriptions :
    { msgDecoder : Json.Decode.Decoder msg
    , subscriptions : model -> Sub msg
    }
    -> Model model msg
    -> Sub (Msg msg)
wrapSubscriptions config model =
    Sub.map Update (config.subscriptions (History.now model.history))


wrapUpdate :
    { msgDecoder : Json.Decode.Decoder msg
    , encodeMsg : msg -> Json.Encode.Value
    , update : msg -> model -> ( model, Cmd msg )
    , toPort : Json.Encode.Value -> Cmd (Msg msg)
    }
    -> Msg msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg msg) )
wrapUpdate config msg model =
    case msg of
        Update updateMsg ->
            let
                ( updatedModel, updateCmds ) =
                    config.update updateMsg (History.now model.history)
            in
            ( { model
                | history = History.insert ( updateMsg, updatedModel ) model.history
              }
            , mapUpdate updateCmds
            )

        ResizeViewport viewportSize ->
            ( { model
                | viewportSize = viewportSize
              }
            , Cmd.none
            )


wrapDocument :
    { encodeMsg : msg -> Json.Encode.Value
    , printModel : model -> String
    , view : model -> Browser.Document msg
    }
    -> Model model msg
    -> Browser.Document (Msg msg)
wrapDocument config model =
    let
        { title, body } =
            config.view (History.now model.history)
    in
    { title = title
    , body =
        [ view config.printModel model (List.map (Html.map Update) body)
        ]
    }


wrapHtml :
    { encodeMsg : msg -> Json.Encode.Value
    , printModel : model -> String
    , view : model -> Html msg
    }
    -> Model model msg
    -> Html (Msg msg)
wrapHtml config model =
    view config.printModel model [ Html.map Update (config.view (History.now model.history)) ]


view : (model -> String) -> Model model msg -> List (Html (Msg msg)) -> Html (Msg msg)
view printModel model viewApp =
    View.toBody
        (View.nothing
            :: View.debugger
                { position = model.position }
            :: View.overlay
                printModel
                { size = model.viewportSize
                , model = History.now model.history
                }
            :: viewApp
        )
