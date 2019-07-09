module Page exposing
    ( Page(..)
    , fromString
    , fromUrl
    , p
    , title
    , toString
    , toUrl
    , view
    , viewFeatures
    , viewGoals
    , viewMario
    , viewSplash
    , viewStart
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Mario
import Style
import Url exposing (Url)


type Page
    = Splash
    | Start
    | Features
    | Goals
    | Mario



-- Conversion


title : Page -> String
title page =
    case page of
        Start ->
            "get started"

        _ ->
            Maybe.withDefault "" (toString page)


fromUrl : Url -> Page
fromUrl =
    .fragment
        >> Maybe.andThen fromString
        >> Maybe.withDefault Splash


toUrl : Page -> String
toUrl =
    toString
        >> Maybe.map ((++) "#")
        >> Maybe.withDefault "/"


fromString : String -> Maybe Page
fromString text =
    case text of
        "start" ->
            Just Start

        "features" ->
            Just Features

        "goals" ->
            Just Goals

        "mario" ->
            Just Mario

        _ ->
            Just Splash


toString : Page -> Maybe String
toString page =
    case page of
        Start ->
            Just "start"

        Features ->
            Just "features"

        Goals ->
            Just "goals"

        Mario ->
            Just "mario"

        Splash ->
            Nothing



-- View


view :
    { model
        | mario : Mario.Model
        , marioHeight : Int
        , page : Page
    }
    -> Element Mario.Msg
view model =
    el
        [ height fill
        , width fill
        ]
        (case model.page of
            Splash ->
                viewSplash

            Start ->
                viewStart

            Features ->
                viewFeatures

            Goals ->
                viewGoals

            Mario ->
                viewMario model.marioHeight model.mario
        )


viewMario : Int -> Mario.Model -> Element Mario.Msg
viewMario elementHeight mario =
    column [ height fill, width fill ]
        [ el
            [ centerX
            , height fill
            , Font.color Style.muted
            , Font.size 15
            , paddingEach
                { top = 40
                , bottom = 0
                , left = 0
                , right = 0
                }
            ]
            (text "use arrow-keys")
        , el [ height (px elementHeight) ]
            (html (Mario.view mario))
        ]


viewGoals : Element msg
viewGoals =
    textColumn
        [ Font.size 15
        , padding 40
        , spacing 15
        , centerX
        ]
        [ p "The overarching goal is to close the loop between writing Elm code and interacting with it."
        , p "This concretely means:"
        , column
            [ width fill
            , Font.bold
            , spacing 10
            ]
            [ row [ spacing 10 ]
                [ column [ Border.rounded 5, height fill, width fill, padding 20, Background.color Style.lightGray ]
                    [ p "Code changes should be reflected immediately in a running instance of its application."
                    ]
                , column [ Border.rounded 5, height fill, width fill, padding 20, Background.color Style.lightGray ]
                    [ p "Code changes should only break application-state if necessary, and if enabled, only partially."
                    ]
                ]
            , row [ spacing 10 ]
                [ column [ Border.rounded 5, height fill, width fill, padding 20, Background.color Style.lightGray ]
                    [ p "Application-states should be effortless to inspect, navigate, persist and share."
                    ]
                , column [ Border.rounded 5, height fill, width fill, padding 20, Background.color Style.lightGray ]
                    [ p "Guidance from the compiler should be right in front of you when you make mistakes."
                    ]
                ]
            ]
        , p "The boilerplate necessary to enable the first four goals should be minimal, and if needed be, incremental."
        , p "Going through the loop of compiling code, reloading browser windows and getting the application into the right state costs seconds, but those seconds are spent at such a frequency that writing interactive applications is time-consuming."
        , p "I strongly believe that the optimal design and implementation of these goals will transform how we build our applications."
        ]


viewFeatures : Element msg
viewFeatures =
    textColumn
        [ Font.size 15
        , padding 40
        , spacing 15
        , centerX
        ]
        [ p "The Elm architecture is essential to this project. Since programs are constructed using a few simple functions, they can be embedded and controlled."
        , column [ spacing 5 ]
            [ el [ Font.bold ] (text "Automatic Message Replay")
            ]
        , p "Any previous state of an application can be recomputed using the initial state and a list of messages folded by an update-function. These state-transitions happen independently of their environment - no commands are sent during replay, only the state changes. This is very useful to:"
        , column [ padding 15, spacing 15 ]
            [ p "Stop time to look at what happened."
            , p "Go to a previous point in time to try something else."
            , p "Reload without losing the state of your application."
            , p "Reload with code changes but only partial (or no) loss of application state."
            ]
        , el [ Font.bold ] (text "Sessions")
        , p "When you open an application using these devtools, you start a session. Sessions keep track of every way you interact with the application, which is the basis for returning to a similar state later, even if code was changed."
        , column [ padding 15, spacing 15 ]
            [ el [ Font.bold ] (text "Bug Reports")
            , p "Sessions can be filed as bug-reports, that easily reproduce whatever needs to be looked at. They download with the current date in the filename. There's a textarea for description too."
            , el [ Font.bold ] (text "App Development")
            , p "Recording interactions rather than state means that applications can load interactions from previous versions of their code, and reproduce a predictably smiliar state."
            , column
                [ padding 15, spacing 15 ]
                [ p "If you changed your view-function, you will reload to exactly where you were, with those changes you made, applied immediately."
                , p "If you changed your subscriptions-, update- or init-function(s), you will reload into a different state. The state is based directly on your modifications to the code, which means you get to tweak functionality with immediate feedback. This is also true for modification to the model-type."
                , p "If changes are made to the message-type, there's a couple of options. Append-only changes always reload you to the same state as before. If a message-constructor is changed or removed, there's a couple of options:"
                , column
                    [ padding 15, spacing 15 ]
                    [ p "You can choose to restart the application as a message might be incompatible with the application. This is the most common experience for me in web-development."
                    , p "Another option is to automatically replay messages and simply skip the incompatible. This can be good sometimes, but can lead to some jarring app-states."
                    , el [ paddingXY 15 0 ] <| p "'view account' doesn't make much sense if 'log in' is skipped."
                    , p "Finally you can automatically replay until the first incompatible message. This is a really great default, where you reliably get to where you were between changes to code."
                    , el [ paddingXY 15 0 ] <| p "'view account' never happens if you can't get past 'log in'."
                    ]
                ]
            , p "By recording interactions rather than state, Elm applications can be written while running, and you will only ever have impared (but predictable) replay of messages when you modify the message-type."
            ]
        ]


viewSplash : Element msg
viewSplash =
    el [ centerX, centerY ] (text "Splash Page")


viewStart : Element msg
viewStart =
    el [ centerX, centerY ] (text "Start Page")


p : String -> Element msg
p content =
    paragraph [] [ text content ]
