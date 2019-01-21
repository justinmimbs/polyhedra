module Brush exposing (Brush, Point2D, init, onStart, subscriptions, update)

import Browser.Events
import Json.Decode as Decode exposing (Decoder)
import Svg
import Svg.Attributes
import Svg.Events


type alias Brush =
    { from : Point2D
    , to : Point2D
    }


type alias Point2D =
    { x : Float
    , y : Float
    }



--


init : Point2D -> Brush
init point =
    { from = point, to = point }


update : Point2D -> Brush -> Brush
update point { from } =
    { from = from, to = point }



-- events


onStart : (Point2D -> msg) -> Svg.Attribute msg
onStart started =
    Svg.Events.preventDefaultOn
        "mousedown"
        (decodeMousePosition |> Decode.map (\point -> ( started point, True )))


subscriptions : (Point2D -> msg) -> msg -> Sub msg
subscriptions moved ended =
    Sub.batch
        [ Browser.Events.onMouseMove (Decode.map moved decodeMousePosition)
        , Browser.Events.onMouseUp (Decode.succeed ended)
        , Browser.Events.onVisibilityChange (always ended)
        ]


decodeMousePosition : Decoder Point2D
decodeMousePosition =
    Decode.map2 Point2D
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)
