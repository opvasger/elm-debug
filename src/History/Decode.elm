module History.Decode exposing
    ( Strategy(..)
    , encodeStrategy
    , fromString
    , loopStrategy
    , strategies
    , strategyDecoder
    , strategyToHistoryDecoder
    , strategyToString
    )

import Help
import History exposing (History)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Strategy
    = NoErrors
    | UntilError
    | SkipErrors


encodeStrategy : Strategy -> Encode.Value
encodeStrategy =
    strategyToString >> Encode.string


strategyToString : Strategy -> String
strategyToString strategy =
    case strategy of
        NoErrors ->
            "NoErrors"

        UntilError ->
            "UntilError"

        SkipErrors ->
            "SkipErrors"


fromString : String -> Result String Strategy
fromString text =
    case text of
        "NoErrors" ->
            Ok NoErrors

        "UntilError" ->
            Ok UntilError

        "SkipErrors" ->
            Ok SkipErrors

        _ ->
            strategies
                |> List.map strategyToString
                |> List.intersperse "', '"
                |> List.foldl (++) ""
                |> (++) ("Expected '" ++ text ++ "' to be one of '")
                |> Err


strategyDecoder : Decoder Strategy
strategyDecoder =
    Decode.andThen
        (fromString >> Result.map Decode.succeed >> Help.unwrapResult Decode.fail)
        Decode.string


strategies : List Strategy
strategies =
    Help.enumerate loopStrategy NoErrors


loopStrategy : Strategy -> Strategy
loopStrategy strategy =
    case strategy of
        NoErrors ->
            UntilError

        UntilError ->
            SkipErrors

        SkipErrors ->
            NoErrors


strategyToHistoryDecoder :
    Strategy
    -> (msg -> model -> model)
    -> Decoder msg
    -> model
    -> Decoder (History model msg)
strategyToHistoryDecoder strategy =
    case strategy of
        NoErrors ->
            History.noErrorsDecoder

        UntilError ->
            History.untilErrorDecoder

        SkipErrors ->
            History.skipErrorsDecoder
