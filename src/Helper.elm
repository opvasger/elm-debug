module Helper exposing
    ( enumerate
    , resultToTask
    , unwrapResult
    , updateModel
    , withoutCmd
    )

import Task exposing (Task)



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
        current :: []

    else
        current :: enumerateHelper loop init next
