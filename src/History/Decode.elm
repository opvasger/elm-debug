module History.Decode exposing
    ( Strategy(..)
    , encodeStrategy
    , fromStrategy
    , strategyDecoder
    )

import History exposing (History)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Strategy
    = NoErrors
    | UntilError
    | SkipErrors


encodeStrategy : Strategy -> Encode.Value
encodeStrategy strategy =
    case strategy of
        NoErrors ->
            Encode.string "NoErrors"

        UntilError ->
            Encode.string "UntilError"

        SkipErrors ->
            Encode.string "SkipErrors"


strategyDecoder : Decoder Strategy
strategyDecoder =
    Decode.andThen
        (\text ->
            case text of
                "NoErrors" ->
                    Decode.succeed NoErrors

                "UntilError" ->
                    Decode.succeed UntilError

                "SkipErrors" ->
                    Decode.succeed SkipErrors

                _ ->
                    "Expected "
                        ++ text
                        ++ " to be one of 'NoError', UntilError, or 'SkipErrors''"
                        |> Decode.fail
        )
        Decode.string


fromStrategy :
    Strategy
    -> (msg -> model -> model)
    -> Decoder msg
    -> model
    -> Decoder (History model msg)
fromStrategy strategy =
    case strategy of
        NoErrors ->
            History.noErrorsDecoder

        UntilError ->
            History.untilErrorDecoder

        SkipErrors ->
            History.skipErrorsDecoder
