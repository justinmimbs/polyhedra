module Brush exposing (Brush, Point2D, init, onStart, subscriptions, touchEnd, touchMove, touchStart, update)

import Browser.Events
import Html
import Html.Events
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
        (decodePosition |> Decode.map (\point -> ( started point, True )))


subscriptions : (Point2D -> msg) -> msg -> Sub msg
subscriptions moved ended =
    Sub.batch
        [ Browser.Events.onMouseMove (Decode.map moved decodePosition)
        , Browser.Events.onMouseUp (Decode.succeed ended)
        , Browser.Events.onVisibilityChange (always ended)
        ]


decodePosition : Decoder Point2D
decodePosition =
    Decode.map2 Point2D
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)



-- touch


touchStart : (Point2D -> msg) -> Svg.Attribute msg
touchStart started =
    Svg.Events.preventDefaultOn
        "touchstart"
        (decodeTouch |> Decode.map (\point -> ( started point, True )))


touchMove : (Point2D -> msg) -> Html.Attribute msg
touchMove moved =
    Html.Events.on
        "touchmove"
        (decodeTouch |> Decode.map moved)


touchEnd : msg -> Html.Attribute msg
touchEnd msg =
    Html.Events.on
        "touchend"
        (Decode.succeed msg)


decodeTouch : Decoder Point2D
decodeTouch =
    Decode.field "touches"
        (Decode.field "length" Decode.int
            |> Decode.andThen
                (\length ->
                    if length == 1 then
                        Decode.field "0" decodePosition

                    else
                        Decode.fail "touches.length /= 1"
                )
        )
