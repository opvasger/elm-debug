module Position exposing (Position, onMouseMove)

import Browser.Events as Be
import Json.Decode as Jd


type alias Position =
    { left : Int
    , top : Int
    }


onMouseMove : (Position -> msg) -> Sub msg
onMouseMove msg =
    Sub.map msg
        (Be.onMouseMove
            (Jd.map2
                Position
                (Jd.field "clientX" Jd.int)
                (Jd.field "clientY" Jd.int)
            )
        )
