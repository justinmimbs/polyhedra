module Slider exposing (Slider, applyBrush, init, value, view)

import Brush exposing (Brush, Point2D)
import Svg exposing (Svg)
import Svg.Attributes


type alias Slider =
    { length : Float
    , val : Float
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
view brushStarted maybeBrush ({ length } as slider) =
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
        , Svg.Attributes.transform "translate(0, 0.5)"
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
            , Svg.Attributes.r "11"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.class "thumb-toucharea"
            , Svg.Attributes.cx xPosition
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "15"
            , Brush.onStart brushStarted
            ]
            []
        ]
