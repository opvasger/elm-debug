module Size exposing
    ( Size
    , decoder
    , encode
    , getViewport
    , onResizeViewport
    )

import Browser.Dom
import Browser.Events
import Json.Decode as Jd
import Json.Encode as Je
import Task exposing (Task)


type alias Size =
    { width : Int
    , height : Int
    }



-- Json


encode : Size -> Je.Value
encode { width, height } =
    Je.object
        [ ( "width", Je.int width )
        , ( "height", Je.int height )
        ]


decoder : Jd.Decoder Size
decoder =
    Jd.map2 Size
        (Jd.field "width" Jd.int)
        (Jd.field "height" Jd.int)



-- Browser


onResizeViewport : Sub Size
onResizeViewport =
    Browser.Events.onResize Size


getViewport : Task x Size
getViewport =
    Task.map fromViewport Browser.Dom.getViewport


fromViewport : Browser.Dom.Viewport -> Size
fromViewport { viewport } =
    Size (round viewport.width) (round viewport.height)
