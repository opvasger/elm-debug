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
import Drag
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Position exposing (Position)
import Size exposing (Size)
import Task


type alias Configuration appModel appMsg =
    { printModel : appModel -> String
    , encodeMsg : appMsg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder appMsg
    , toPort : Json.Encode.Value -> Cmd (Msg appMsg)
    }


type alias Program appModel appMsg =
    Platform.Program Json.Decode.Value (Model appModel appMsg) (Msg appMsg)


type alias Model appModel appMsg =
    { history : History appModel appMsg
    , debuggerDrag : Drag.Model
    , viewportSize : Size
    , isModelOverlayed : Bool
    }


type Msg appMsg
    = UpdateApp appMsg
    | ResizeViewport Size
    | DragDebugger Drag.Msg


mapAppUpdate : Cmd appMsg -> Cmd (Msg appMsg)
mapAppUpdate =
    Cmd.map UpdateApp


wrapMsg : (a -> appMsg) -> a -> Msg appMsg
wrapMsg toMsg =
    UpdateApp << toMsg


wrapInit :
    { model : appModel
    , cmds : Cmd appMsg
    , flags : Json.Decode.Value
    , msgDecoder : Json.Decode.Decoder appMsg
    , update : appMsg -> appModel -> ( appModel, Cmd appMsg )
    }
    -> ( Model appModel appMsg, Cmd (Msg appMsg) )
wrapInit config =
    ( { history = History.init config.model
      , debuggerDrag = Drag.init
      , viewportSize = Size 0 0
      , isModelOverlayed = False
      }
    , Cmd.batch
        [ mapAppUpdate config.cmds
        , Task.perform ResizeViewport Size.getViewportSize
        ]
    )


wrapSubscriptions :
    { msgDecoder : Json.Decode.Decoder appMsg
    , subscriptions : appModel -> Sub appMsg
    }
    -> Model appModel appMsg
    -> Sub (Msg appMsg)
wrapSubscriptions config model =
    Sub.batch
        [ Sub.map UpdateApp (config.subscriptions (History.now model.history))
        , Sub.map DragDebugger (Drag.subscriptions model.debuggerDrag)
        ]


wrapUpdate :
    { msgDecoder : Json.Decode.Decoder appMsg
    , encodeMsg : appMsg -> Json.Encode.Value
    , update : appMsg -> appModel -> ( appModel, Cmd appMsg )
    , toPort : Json.Encode.Value -> Cmd (Msg appMsg)
    }
    -> Msg appMsg
    -> Model appModel appMsg
    -> ( Model appModel appMsg, Cmd (Msg appMsg) )
wrapUpdate config msg model =
    case msg of
        UpdateApp appMsg ->
            let
                ( appModel, appCmd ) =
                    config.update appMsg (History.now model.history)
            in
            ( { model
                | history = History.insert ( appMsg, appModel ) model.history
              }
            , mapAppUpdate appCmd
            )

        ResizeViewport viewportSize ->
            ( { model
                | viewportSize = viewportSize
              }
            , Cmd.none
            )

        DragDebugger debuggerDrag ->
            ( { model
                | debuggerDrag = Drag.update debuggerDrag model.debuggerDrag
              }
            , Cmd.none
            )


wrapDocument :
    { encodeMsg : appMsg -> Json.Encode.Value
    , printModel : appModel -> String
    , view : appModel -> Browser.Document appMsg
    }
    -> Model appModel appMsg
    -> Browser.Document (Msg appMsg)
wrapDocument config appModel =
    let
        { title, body } =
            config.view (History.now appModel.history)
    in
    { title = title
    , body =
        [ view config.printModel appModel (List.map (Html.map UpdateApp) body)
        ]
    }


wrapHtml :
    { encodeMsg : appMsg -> Json.Encode.Value
    , printModel : appModel -> String
    , view : appModel -> Html appMsg
    }
    -> Model appModel appMsg
    -> Html (Msg appMsg)
wrapHtml config appModel =
    view config.printModel appModel [ Html.map UpdateApp (config.view (History.now appModel.history)) ]


view : (appModel -> String) -> Model appModel appMsg -> List (Html (Msg appMsg)) -> Html (Msg appMsg)
view printModel model viewApp =
    View.selectable True
        []
        (View.viewNothing
            :: View.viewDebugger
                { position = model.debuggerDrag.position
                , onMouseDown = Drag.enable DragDebugger
                }
            :: View.viewOverlay
                printModel
                { height = model.viewportSize.height
                , model = History.now model.history
                , isEnabled = model.isModelOverlayed
                }
            :: viewApp
        )
