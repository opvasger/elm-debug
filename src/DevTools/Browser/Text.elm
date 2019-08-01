module DevTools.Browser.Text exposing
    ( collapseWindowTitle
    , commentsPageTitle
    , commentsPlaceholder
    , defaultFileName
    , dissmissWindowTitle
    , downloadSessionTitle
    , expandWindowTitle
    , jsonMimeType
    , messagesPageTitle
    , noCommentsPlaceholder
    , noCommentsTitle
    , noSettingsPlaceholder
    , noSettingsTitle
    , pauseAppTitle
    , printFileName
    , printModelViewTitle
    , printUtcDate
    , replayRangeTitle
    , restartAppTitle
    , settingsPageTitle
    , startAppTitle
    , uploadSessionTitle
    )

import Time


defaultFileName : String
defaultFileName =
    "devtools-session"


messagesPageTitle : String
messagesPageTitle =
    "View messages"


commentsPageTitle : String
commentsPageTitle =
    "View comments"


settingsPageTitle : String
settingsPageTitle =
    "View settings"


replayRangeTitle : String
replayRangeTitle =
    "Replay application states"


startAppTitle : String
startAppTitle =
    "Start the application"


pauseAppTitle : String
pauseAppTitle =
    "Pause the application"


commentsPlaceholder : String
commentsPlaceholder =
    """Take a moment to describe what you're doing!

 🐞 Did you encounter a bug
       you want to report?

 💭 Do you want to write a
       note before leaving?

 ⌗   Which Git-branch/commit
       is this session for?
"""


noCommentsTitle : String
noCommentsTitle =
    "No comments included."


noCommentsPlaceholder : String
noCommentsPlaceholder =
    """ 💡 You can edit these fields
       if you unlock features to
       export or cache your 
       session.
"""


noSettingsTitle : String
noSettingsTitle =
    "No settings available."


noSettingsPlaceholder : String
noSettingsPlaceholder =
    """ 💡 You can configure how
       session-caching works
       if you unlock the feature.
"""


dissmissWindowTitle : String
dissmissWindowTitle =
    "Dismiss the window"


restartAppTitle : String
restartAppTitle =
    "Restart the application"


collapseWindowTitle : String
collapseWindowTitle =
    "Collapse the window"


expandWindowTitle : String
expandWindowTitle =
    "Expand the window"


downloadSessionTitle : String
downloadSessionTitle =
    "Download session"


uploadSessionTitle : String
uploadSessionTitle =
    "Upload session"


printModelViewTitle : Bool -> String
printModelViewTitle isModelVisible =
    if isModelVisible then
        "Hide model"

    else
        "View model"


jsonMimeType : String
jsonMimeType =
    "application/json"


printFileName : Time.Posix -> String -> String
printFileName time title =
    let
        fileTitle =
            case title of
                "" ->
                    defaultFileName

                _ ->
                    title
    in
    fileTitle ++ "." ++ printUtcDate time ++ ".json"


printUtcDate : Time.Posix -> String
printUtcDate time =
    List.foldl (++)
        ""
        [ Time.toYear Time.utc time
            |> String.fromInt
            |> String.right 2
        , "-"
        , Time.toMonth Time.utc time
            |> monthToInt
            |> String.fromInt
            |> String.padLeft 2 '0'
        , "-"
        , Time.toDay Time.utc time
            |> String.fromInt
            |> String.padLeft 2 '0'
        ]



-- Helpers


monthToInt : Time.Month -> Int
monthToInt month =
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
