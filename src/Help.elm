module Help exposing
    ( backgroundColor
    , borderStyle
    , enumerate
    , monthNumber
    , printUtcTime
    , px
    , replaceEmptyWith
    , resultToTask
    , symbolColor
    , unwrapResult
    , updateModel
    , withoutCmd
    , zIndexMax
    )

import Task exposing (Task)
import Time



-- Style


px : Int -> String
px n =
    String.fromInt n ++ "px"


backgroundColor : String
backgroundColor =
    "#f3f3f3"


borderStyle : String
borderStyle =
    "1px solid #cccccc"


symbolColor : String
symbolColor =
    "#6e6e6e"


zIndexMax : Int
zIndexMax =
    2147483647



-- String


replaceEmptyWith : String -> String -> String
replaceEmptyWith replace text =
    if String.length (String.trim text) < 1 then
        replace

    else
        text



-- Time


printUtcTime : Time.Posix -> String
printUtcTime time =
    List.foldl (++)
        ""
        [ Time.toYear Time.utc time
            |> String.fromInt
            |> String.right 2
        , "-"
        , Time.toMonth Time.utc time
            |> monthNumber
            |> String.fromInt
            |> String.padLeft 2 '0'
        , "-"
        , Time.toDay Time.utc time
            |> String.fromInt
            |> String.padLeft 2 '0'
        ]


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12



-- Result


resultToTask : Result err ok -> Task err ok
resultToTask result =
    case result of
        Ok value ->
            Task.succeed value

        Err error ->
            Task.fail error


unwrapResult : (err -> ok) -> Result err ok -> ok
unwrapResult fromError result =
    case result of
        Ok value ->
            value

        Err error ->
            fromError error



-- Cmd


withoutCmd : model -> ( model, Cmd msg )
withoutCmd model =
    ( model, Cmd.none )


updateModel : (msg -> model -> ( model, Cmd msg )) -> msg -> model -> model
updateModel update msg model =
    Tuple.first (update msg model)



-- Enumeration


enumerate : (a -> a) -> a -> List a
enumerate loop init =
    enumerateHelper loop init init


enumerateHelper : (a -> a) -> a -> a -> List a
enumerateHelper loop init current =
    let
        next =
            loop current
    in
    if next == init then
        [ current ]

    else
        current :: enumerateHelper loop init next
