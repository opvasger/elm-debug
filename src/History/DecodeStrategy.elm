module History.DecodeStrategy exposing
    ( DecodeStrategy(..)
    , all
    , decoder
    , encode
    , fromString
    , loop
    , toHistoryDecoder
    , toString
    )

import Helper
import History exposing (History)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type DecodeStrategy
    = NoErrors
    | UntilError
    | SkipErrors


encode : DecodeStrategy -> Encode.Value
encode =
    toString >> Encode.string


toString : DecodeStrategy -> String
toString strategy =
    case strategy of
        NoErrors ->
            "NoErrors"

        UntilError ->
            "UntilError"

        SkipErrors ->
            "SkipErrors"


fromString : String -> Result String DecodeStrategy
fromString text =
    case text of
        "NoErrors" ->
            Ok NoErrors

        "UntilError" ->
            Ok UntilError

        "SkipErrors" ->
            Ok SkipErrors

        _ ->
            all
                |> List.map toString
                |> List.intersperse "', '"
                |> List.foldl (++) ""
                |> (++) ("Expected '" ++ text ++ "' to be one of '")
                |> Err


decoder : Decoder DecodeStrategy
decoder =
    Decode.andThen
        (fromString >> Result.map Decode.succeed >> Helper.unwrapResult Decode.fail)
        Decode.string


all : List DecodeStrategy
all =
    Helper.enumerate loop NoErrors


loop : DecodeStrategy -> DecodeStrategy
loop strategy =
    case strategy of
        NoErrors ->
            UntilError

        UntilError ->
            SkipErrors

        SkipErrors ->
            NoErrors


toHistoryDecoder :
    DecodeStrategy
    -> (msg -> model -> model)
    -> Decoder msg
    -> model
    -> Decoder (History model msg)
toHistoryDecoder strategy =
    case strategy of
        NoErrors ->
            History.noErrorsDecoder

        UntilError ->
            History.untilErrorDecoder

        SkipErrors ->
            History.skipErrorsDecoder
