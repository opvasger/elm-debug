module Size exposing (Size, getViewportSize)

import Browser.Dom as Bd
import Task exposing (Task)


type alias Size =
    { width : Int
    , height : Int
    }


getViewportSize : Task Never Size
getViewportSize =
    Task.map fromViewport Bd.getViewport


fromViewport : Bd.Viewport -> Size
fromViewport { viewport } =
    { width = round viewport.width
    , height = round viewport.height
    }
