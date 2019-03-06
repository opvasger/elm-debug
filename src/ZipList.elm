module ZipList exposing
    ( ZipList
    , backward
    , currentIndex
    , dropHeads
    , filterMap
    , forward
    , head
    , indexedMap
    , insert
    , jsonDecoder
    , jsonEncode
    , length
    , map
    , singleton
    , tail
    , toHead
    , toIndex
    , toList
    , toTail
    , trim
    )

import Json.Decode as Jd
import Json.Encode as Je



-- API


type alias ZipList value =
    { heads : List value
    , current : value
    , tails : List value
    }


tail : ZipList value -> value
tail zl =
    Maybe.withDefault zl.current (tailValue zl.tails)


head : ZipList value -> value
head zl =
    Maybe.withDefault zl.current (tailValue zl.heads)


trim : Int -> ZipList value -> ZipList value
trim newLength zl =
    if length zl <= newLength then
        zl

    else if List.length zl.heads >= List.length zl.tails then
        trim newLength { zl | heads = List.take (List.length zl.heads - 1) zl.heads }

    else
        trim newLength { zl | tails = List.take (List.length zl.tails - 1) zl.tails }


currentIndex : ZipList value -> Int
currentIndex zl =
    List.length zl.tails


map : (value -> a) -> ZipList value -> ZipList a
map op zl =
    { heads = List.map op zl.heads
    , current = op zl.current
    , tails = List.map op zl.tails
    }


filterMap : (value -> Maybe b) -> ZipList value -> Maybe (ZipList b)
filterMap op zl =
    case ( op zl.current, Maybe.andThen op (List.head zl.heads), Maybe.andThen op (List.head zl.tails) ) of
        ( Just current, _, _ ) ->
            Just
                { heads = List.filterMap op zl.heads
                , current = current
                , tails = List.filterMap op zl.tails
                }

        ( Nothing, Just value, _ ) ->
            Just
                { heads = List.filterMap op (Maybe.withDefault [] (List.tail zl.heads))
                , current = value
                , tails = List.filterMap op zl.tails
                }

        ( Nothing, _, Just value ) ->
            Just
                { heads = List.filterMap op zl.heads
                , current = value
                , tails = List.filterMap op (Maybe.withDefault [] (List.tail zl.tails))
                }

        ( Nothing, Nothing, Nothing ) ->
            Nothing


indexedMap : (Int -> value -> a) -> ZipList value -> ZipList a
indexedMap op zl =
    { heads = List.indexedMap (\headIndex value -> op (headIndex + 1 + List.length zl.tails) value) zl.heads
    , current = op (List.length zl.tails) zl.current
    , tails = List.reverse (List.indexedMap op (List.reverse zl.tails))
    }


insert : value -> ZipList value -> ZipList value
insert value zl =
    { zl
        | heads = zl.heads
        , current = value
        , tails = zl.current :: zl.tails
    }


dropHeads : ZipList value -> ZipList value
dropHeads zl =
    { zl | heads = [] }


length : ZipList value -> Int
length zl =
    List.length zl.heads + 1 + List.length zl.tails


singleton : value -> ZipList value
singleton value =
    { heads = []
    , current = value
    , tails = []
    }


toList : ZipList value -> List value
toList zl =
    List.reverse zl.tails ++ [ zl.current ] ++ zl.heads


toIndex : Int -> ZipList value -> ZipList value
toIndex index zl =
    let
        diff =
            List.length zl.tails - index

        op =
            if diff > 0 then
                backward

            else
                forward
    in
    zl
        |> List.foldl
            (>>)
            identity
            (List.repeat (abs diff) op)


toHead : ZipList value -> ZipList value
toHead zl =
    case zl.heads of
        [] ->
            zl

        newCurrent :: newHeads ->
            toHead
                { zl
                    | heads = newHeads
                    , current = newCurrent
                    , tails = zl.current :: zl.tails
                }


toTail : ZipList value -> ZipList value
toTail zl =
    case zl.tails of
        [] ->
            zl

        newCurrent :: newTails ->
            toTail
                { zl
                    | heads = zl.current :: zl.heads
                    , current = newCurrent
                    , tails = newTails
                }


forward : ZipList value -> ZipList value
forward zl =
    case zl.heads of
        [] ->
            zl

        newCurrent :: newHeads ->
            { heads = newHeads
            , current = newCurrent
            , tails = zl.current :: zl.tails
            }


backward : ZipList value -> ZipList value
backward zl =
    case zl.tails of
        [] ->
            zl

        newCurrent :: newTails ->
            { heads = zl.current :: zl.heads
            , current = newCurrent
            , tails = newTails
            }


jsonEncode : (value -> Je.Value) -> ZipList value -> Je.Value
jsonEncode encodeValue { heads, current, tails } =
    Je.object
        [ ( "heads", Je.list encodeValue heads )
        , ( "current", encodeValue current )
        , ( "tails", Je.list encodeValue tails )
        ]


jsonDecoder : Jd.Decoder value -> Jd.Decoder (ZipList value)
jsonDecoder valueDecoder =
    Jd.map3
        ZipList
        (Jd.field "heads" (Jd.list valueDecoder))
        (Jd.field "current" valueDecoder)
        (Jd.field "tails" (Jd.list valueDecoder))



-- Internals


tailValue : List value -> Maybe value
tailValue list =
    case list of
        value :: [] ->
            Just value

        _ :: values ->
            tailValue values

        [] ->
            Nothing



--
