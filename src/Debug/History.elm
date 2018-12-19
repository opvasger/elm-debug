module Debug.History exposing (History, init)


type History model msg
    = History model (List msg)


init : model -> History model msg
init model =
    History model []
