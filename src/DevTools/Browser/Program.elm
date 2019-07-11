module DevTools.Browser.Program exposing
    ( Model
    , Msg(..)
    , MsgSource(..)
    , SessionDecodeError(..)
    , init
    , subscriptions
    , update
    , urlUpdate
    , view
    )

import Browser
import File exposing (File)
import History exposing (History)
import History.Decode
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Throttle
import Time



-- Program


type alias Model model msg =
    { -- App
      history : History model msg
    , initCmd : Cmd msg

    -- Session
    , decodeStrategy : History.Decode.Strategy
    , decodeError : SessionDecodeError
    , cacheThrottle : Throttle.Model

    -- Layout
    , isModelVisible : Bool

    -- Report
    , title : String
    , description : String
    }


type Msg model msg
    = DoNothing
      -- App
    | UpdateApp MsgSource msg
    | RestartApp
    | ReplayApp Int
    | ToggleAppReplay
      -- Session
    | UseDecodeStrategy History.Decode.Strategy
    | DownloadSessionWithDate
    | DownloadSession Time.Posix
    | SelectSessionFile
    | DecodeSession File
    | SessionDecoded (Result Decode.Error (Model model msg))
    | UpdateCacheThrottle
      -- Layout
    | ToggleModelVisible
      -- Report
    | InputTitle String
    | InputDescription String


defaultTitle : String
defaultTitle =
    "devtools-session"


descriptionPlaceholder : String
descriptionPlaceholder =
    """Take a moment to describe what you're doing!

 🐞 Did you encounter a bug
       you want to report?

 💭 Do you want to write a
       note before leaving?

 ⌗   Which Git-branch/commit
       is this session for?
"""


urlUpdate : msg -> Msg model msg
urlUpdate =
    UpdateApp FromUrl


init :
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , msgDecoder : Maybe (Decoder msg)
    , fromCache : Maybe String
    }
    -> ( Model model msg, Cmd (Msg model msg) )
init config =
    ( { history = History.init (Tuple.first config.init)
      , initCmd = Tuple.second config.init
      , decodeError = NoDecodeError
      , decodeStrategy = History.Decode.UntilError
      , cacheThrottle = Throttle.init
      , isModelVisible = False
      , description = ""
      , title = ""
      }
    , Cmd.none
    )


subscriptions :
    { update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , msgDecoder : Maybe (Decoder msg)
    }
    -> Model model msg
    -> Sub (Msg model msg)
subscriptions config model =
    Sub.none


update :
    { update : msg -> model -> ( model, Cmd msg )
    , toCache : Maybe (String -> Cmd (Msg model msg))
    , encodeMsg : Maybe (msg -> Encode.Value)
    , msgDecoder : Maybe (Decoder msg)
    }
    -> Msg model msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg model msg) )
update config msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


view :
    { view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , encodeMsg : Maybe (msg -> Encode.Value)
    , encodeModel : Maybe (model -> Encode.Value)
    }
    -> Model model msg
    -> Browser.Document (Msg model msg)
view config model =
    { title = ""
    , body = []
    }



-- SessionDecodeError


type SessionDecodeError
    = NoDecodeError
    | CacheDecodeError Decode.Error
    | ImportDecodeError Decode.Error



-- MsgSource


type MsgSource
    = FromInit
    | FromUpdate
    | FromSubs
    | FromView
    | FromUrl
