module Slider exposing (Slider, applyBrush, init, value, view)

import Brush exposing (Brush, Point2D)
import Svg exposing (Svg)
import Svg.Attributes


type alias Slider =
    { length : Float
    , position : Float
    }


init : Float -> Float -> Slider
init length val =
    { length = length
    , position = val * length
    }


positionWithBrush : Brush -> Slider -> Float
positionWithBrush { from, to } slider =
    clamp 0 slider.length (slider.position + (to.x - from.x))


position : Maybe Brush -> Slider -> Float
position maybeBrush slider =
    case maybeBrush of
        Just brush ->
            positionWithBrush brush slider

        Nothing ->
            slider.position


value : Maybe Brush -> Slider -> Float
value maybeBrush slider =
    position maybeBrush slider / slider.length


applyBrush : Brush -> Slider -> Slider
applyBrush brush slider =
    { length = slider.length
    , position = positionWithBrush brush slider
    }



-- view


view : (Point2D -> msg) -> Maybe Brush -> Slider -> Svg msg
view brushStarted maybeBrush slider =
    let
        x =
            position maybeBrush slider
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
            [ Svg.Attributes.x1 <| String.fromFloat x
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 <| String.fromFloat slider.length
            , Svg.Attributes.y2 "0"
            ]
            []
        , Svg.line
            [ Svg.Attributes.class "active"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 <| String.fromFloat x
            , Svg.Attributes.y2 "0"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.class "thumb"
            , Svg.Attributes.cx <| String.fromFloat x
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "11.5"
            ]
            []
        , Svg.rect
            [ Svg.Attributes.class "thumb-toucharea"
            , Svg.Attributes.x <| String.fromFloat (x - 20)
            , Svg.Attributes.y "-20"
            , Svg.Attributes.width "40"
            , Svg.Attributes.height "40"
            , Brush.onStart brushStarted
            , Brush.touchStart brushStarted
            ]
            []
        ]
