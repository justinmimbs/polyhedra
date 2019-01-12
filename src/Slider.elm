module Slider exposing (Slider, brushEnd, brushMove, brushStart, init, isBrushing, value, view)

import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


type alias Slider =
    { brushing : Maybe Float
    , length : Float
    , val : Float
    }


init : Float -> Float -> Slider
init =
    Slider Nothing



-- query


value : Slider -> Float
value { val } =
    val


isBrushing : Slider -> Bool
isBrushing { brushing } =
    case brushing of
        Just _ ->
            True

        Nothing ->
            False



-- update actions


type alias Point2D =
    { x : Float
    , y : Float
    }


brushStart : Point2D -> Slider -> Slider
brushStart { x } { length, val } =
    Slider (Just (val * length - x)) length val


brushMove : Point2D -> Slider -> Slider
brushMove { x } ({ length, brushing } as slider) =
    case brushing of
        Just offset ->
            Slider brushing length (clamp 0 1 ((x + offset) / length))

        Nothing ->
            slider


brushEnd : Slider -> Slider
brushEnd { length, val } =
    Slider Nothing length val



-- view


view : (Point2D -> msg) -> Slider -> Svg msg
view tagger ({ length, val } as slider) =
    Svg.g
        [ Svg.Attributes.class <|
            "slider"
                ++ (if slider |> isBrushing then
                        " active"

                    else
                        ""
                   )
        ]
        [ Svg.line
            [ Svg.Attributes.x1 "0"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 <| String.fromFloat length
            , Svg.Attributes.y2 "0"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.cx <| String.fromFloat (val * length)
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "5"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.class "thumb-toucharea"
            , Svg.Attributes.cx <| String.fromFloat (val * length)
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "15"
            , Svg.Events.preventDefaultOn
                "mousedown"
                (decodeMousePosition |> Decode.map (\point -> ( tagger point, True )))
            ]
            []
        ]


decodeMousePosition : Decoder Point2D
decodeMousePosition =
    Decode.map2 Point2D
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)
