module Debug.Browser.History exposing (History, init, insert, now)


type alias Model model msg =
    { initial : model
    , entries : List ( msg, model )
    }


type History model msg
    = History (Model model msg)


init : model -> History model msg
init model =
    History
        { initial = model
        , entries = []
        }


now : History model msg -> model
now (History { initial, entries }) =
    Maybe.withDefault initial (Maybe.map Tuple.second (List.head entries))


insert : ( msg, model ) -> History model msg -> History model msg
insert entry (History model) =
    History { model | entries = entry :: model.entries }
