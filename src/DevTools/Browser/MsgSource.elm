module DevTools.Browser.MsgSource exposing
    ( MsgSource(..)
    , decoder
    , encode
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type MsgSource
    = Init
    | View
    | Update
    | Subscriptions
    | Navigation


encode : MsgSource -> Encode.Value
encode =
    toString >> Encode.string


decoder : Decoder MsgSource
decoder =
    Decode.andThen
        (\text ->
            text
                |> fromString
                |> Maybe.map Decode.succeed
                |> Maybe.withDefault
                    (Decode.fail
                        ("'"
                            ++ text
                            ++ "' is not a valid source of messages"
                        )
                    )
        )
        Decode.string


toString : MsgSource -> String
toString src =
    case src of
        Init ->
            "I"

        View ->
            "V"

        Update ->
            "U"

        Subscriptions ->
            "S"

        Navigation ->
            "N"


fromString : String -> Maybe MsgSource
fromString text =
    case text of
        "I" ->
            Just Init

        "V" ->
            Just View

        "U" ->
            Just Update

        "S" ->
            Just Subscriptions

        "N" ->
            Just Navigation

        _ ->
            Nothing
