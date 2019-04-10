module Brush exposing (Brush, Point2D, onBrush, onStart)

import Browser.Events
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)


type alias Brush =
    { eventType : EventType
    , from : Point2D
    , to : Point2D
    }


type EventType
    = Pointer
    | Mouse
    | Touch


type alias Point2D =
    { x : Float
    , y : Float
    }


init : EventType -> Point2D -> Brush
init eventType point =
    { eventType = eventType
    , from = point
    , to = point
    }


onStart : (Brush -> msg) -> List (Html.Attribute msg)
onStart started =
    [ Html.Events.preventDefaultOn
        "pointerdown"
        (decodePosition |> Decode.map (\point -> ( started (init Pointer point), True )))
    , Html.Events.preventDefaultOn
        "touchstart"
        (decodeTouchPosition |> Decode.map (\point -> ( started (init Touch point), True )))
    , Html.Events.preventDefaultOn
        "mousedown"
        (decodePosition |> Decode.map (\point -> ( started (init Mouse point), True )))
    ]


onBrush : (Brush -> msg) -> msg -> Brush -> List (Html.Attribute msg)
onBrush moved ended brush =
    let
        movedTo : Point2D -> msg
        movedTo point =
            moved
                { eventType = brush.eventType
                , from = brush.from
                , to = point
                }
    in
    case brush.eventType of
        Pointer ->
            [ Html.Events.on "pointermove" (decodePosition |> Decode.map movedTo)
            , Html.Events.on "pointerup" (Decode.succeed ended)
            ]

        Touch ->
            [ Html.Events.on "touchmove" (decodeTouchPosition |> Decode.map movedTo)
            , Html.Events.on "touchend" (Decode.succeed ended)
            ]

        Mouse ->
            [ Html.Events.on "mousemove" (decodePosition |> Decode.map movedTo)
            , Html.Events.on "mouseup" (Decode.succeed ended)
            ]


decodePosition : Decoder Point2D
decodePosition =
    Decode.map2 Point2D
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


decodeTouchPosition : Decoder Point2D
decodeTouchPosition =
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
