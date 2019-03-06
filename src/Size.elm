module Size exposing (Size, fromViewport, init, mapFromInts)

import Browser.Dom exposing (Viewport)


type alias Size =
    { width : Int
    , height : Int
    }


init : Size
init =
    Size 0 0


fromViewport : Viewport -> Size
fromViewport { viewport } =
    { width = round viewport.width
    , height = round viewport.height
    }


mapFromInts : (Size -> msg) -> Int -> Int -> msg
mapFromInts toMsg width height =
    toMsg
        { width = width
        , height = height
        }
