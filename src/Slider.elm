module Slider exposing (Slider, applyBrush, init, value, view)

import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


type alias Slider =
    { length : Float
    , val : Float
    }


type alias Brush =
    { from : Point2D
    , to : Point2D
    }


type alias Point2D =
    { x : Float
    , y : Float
    }


init : Float -> Float -> Slider
init =
    Slider


valueWithBrush : Brush -> Slider -> Float
valueWithBrush { from, to } { length, val } =
    let
        valDelta =
            (to.x - from.x) / length
    in
    clamp 0 1 (val + valDelta)


value : Maybe Brush -> Slider -> Float
value maybeBrush slider =
    case maybeBrush of
        Just brush ->
            valueWithBrush brush slider

        Nothing ->
            slider.val


applyBrush : Brush -> Slider -> Slider
applyBrush brush slider =
    { length = slider.length
    , val = valueWithBrush brush slider
    }



-- view


view : (Point2D -> msg) -> Maybe Brush -> Slider -> Svg msg
view tagger maybeBrush ({ length } as slider) =
    let
        xPosition =
            String.fromFloat (value maybeBrush slider * length)
    in
    Svg.g
        [ Svg.Attributes.class <|
            "slider"
                ++ (case maybeBrush of
                        Just _ ->
                            " pressed"

                        Nothing ->
                            ""
                   )
        ]
        [ Svg.line
            [ Svg.Attributes.x1 xPosition
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 <| String.fromFloat length
            , Svg.Attributes.y2 "0"
            ]
            []
        , Svg.line
            [ Svg.Attributes.class "active"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 xPosition
            , Svg.Attributes.y2 "0"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.class "thumb"
            , Svg.Attributes.cx xPosition
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "5"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.class "thumb-toucharea"
            , Svg.Attributes.cx xPosition
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "15"
            , Svg.Events.preventDefaultOn
                "mousedown"
                (decodeMousePosition |> Decode.map (\point -> ( tagger point, True )))
            ]
            []
        , Svg.circle
            [ Svg.Attributes.class "thumb-outline"
            , Svg.Attributes.cx xPosition
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "6"
            ]
            []
        ]


decodeMousePosition : Decoder Point2D
decodeMousePosition =
    Decode.map2 Point2D
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)
