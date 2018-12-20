module Size exposing (Size, getViewportSize)

import Browser.Dom as Dom
import Task exposing (Task)


type alias Size =
    { width : Int
    , height : Int
    }


getViewportSize : Task Never Size
getViewportSize =
    Task.map fromViewport Dom.getViewport


fromViewport : Dom.Viewport -> Size
fromViewport { viewport } =
    { width = round viewport.width
    , height = round viewport.height
    }
