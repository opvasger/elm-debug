module Position exposing
    ( Position
    , add
    , decoder
    , encode
    , mouseDecoder
    , sub
    , zero
    )

import Json.Decode as Jd
import Json.Encode as Je


type alias Position =
    { left : Int
    , top : Int
    }


zero : Position
zero =
    Position 0 0


add : Position -> Position -> Position
add a b =
    { left = b.left + a.left
    , top = b.top + a.top
    }


sub : Position -> Position -> Position
sub a b =
    { left = b.left - a.left
    , top = b.top - a.top
    }


encode : Position -> Je.Value
encode { left, top } =
    Je.object
        [ ( "left", Je.int left )
        , ( "top", Je.int top )
        ]


decoder : Jd.Decoder Position
decoder =
    Jd.map2 Position
        (Jd.field "left" Jd.int)
        (Jd.field "top" Jd.int)


mouseDecoder : Jd.Decoder Position
mouseDecoder =
    Jd.map2 Position
        (Jd.field "clientX" Jd.int)
        (Jd.field "clientY" Jd.int)
