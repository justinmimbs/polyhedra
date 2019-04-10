module Slider exposing (Slider, applyBrush, init, value, view)

import Brush exposing (Brush, Point2D)
import Svg exposing (Svg)
import Svg.Attributes


type alias Slider =
    { length : Float
    , position : Float
    , breakpoints : List Point2D
    }


init : Float -> Float -> Slider
init length val =
    { length = length
    , position = val * length
    , breakpoints = centerSnapBreakpoints length 8 7
    }


positionWithBrush : Brush -> Slider -> Float
positionWithBrush { from, to } slider =
    clamp 0 slider.length (slider.position + (to.x - from.x))


position : Maybe Brush -> Slider -> Float
position maybeBrush slider =
    (case maybeBrush of
        Just brush ->
            positionWithBrush brush slider

        Nothing ->
            slider.position
    )
        |> piecewiseLinearInterpolate identity slider.breakpoints


value : Maybe Brush -> Slider -> Float
value maybeBrush slider =
    position maybeBrush slider / slider.length


applyBrush : Brush -> Slider -> Slider
applyBrush brush slider =
    { slider | position = positionWithBrush brush slider }



-- view


view : (Brush -> msg) -> Maybe Brush -> Slider -> Svg msg
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
            (Brush.onStart brushStarted
                ++ [ Svg.Attributes.class "thumb-toucharea"
                   , Svg.Attributes.x <| String.fromFloat (x - 20)
                   , Svg.Attributes.y "-20"
                   , Svg.Attributes.width "40"
                   , Svg.Attributes.height "40"
                   ]
            )
            []
        ]



-- snap


centerSnapBreakpoints : Float -> Float -> Float -> List Point2D
centerSnapBreakpoints length ramp notch =
    let
        center =
            length / 2
    in
    [ Point2D (center - notch - ramp) (center - notch - ramp)
    , Point2D (center - notch) center
    , Point2D (center + notch) center
    , Point2D (center + notch + ramp) (center + notch + ramp)
    ]



-- Interpolate


{-| Expect x to monotonically increase, to ensure function x -> y.
-}
piecewiseLinearInterpolate : (Float -> Float) -> List Point2D -> Float -> Float
piecewiseLinearInterpolate default points x =
    case findEndpoints x points of
        Just ( a, b ) ->
            linearInterpolate a b x

        Nothing ->
            default x


findEndpoints : Float -> List Point2D -> Maybe ( Point2D, Point2D )
findEndpoints x points =
    case points of
        a :: rest ->
            if a.x <= x then
                findEndpointsHelp x a rest

            else
                Nothing

        [] ->
            Nothing


findEndpointsHelp : Float -> Point2D -> List Point2D -> Maybe ( Point2D, Point2D )
findEndpointsHelp x a points =
    case points of
        b :: rest ->
            if x < b.x then
                Just ( a, b )

            else
                findEndpointsHelp x b rest

        [] ->
            Nothing


linearInterpolate : Point2D -> Point2D -> Float -> Float
linearInterpolate a b x =
    a.y + ((x - a.x) / (b.x - a.x)) * (b.y - a.y)
