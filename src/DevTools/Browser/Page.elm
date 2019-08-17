module DevTools.Browser.Page exposing (Page(..), decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Page
    = Comments
    | Settings
    | Messages


encode : Page -> Encode.Value
encode page =
    Encode.string <|
        case page of
            Comments ->
                "report"

            Settings ->
                "settings"

            Messages ->
                "messages"


decoder : Decoder Page
decoder =
    Decode.andThen
        (\text ->
            case text of
                "report" ->
                    Decode.succeed Comments

                "messages" ->
                    Decode.succeed Messages

                "settings" ->
                    Decode.succeed Settings

                _ ->
                    Decode.fail ("'" ++ text ++ "' cannot be decoded into a page")
        )
        Decode.string
