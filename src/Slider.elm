module Slider exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( init 300 120, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    Slider



-- slider


type alias Slider =
    { brushing : Maybe Float
    , length : Float
    , position : Float
    }


init : Float -> Float -> Slider
init =
    Slider Nothing


isBrushing : Slider -> Bool
isBrushing { brushing } =
    case brushing of
        Just _ ->
            True

        Nothing ->
            False


brushStart : Point2D -> Slider -> Slider
brushStart { x } { length, position } =
    Slider (Just (position - x)) length position


brushMove : Point2D -> Slider -> Slider
brushMove { x } ({ brushing, length } as slider) =
    case brushing of
        Just offset ->
            Slider brushing length (clamp 0 length (x + offset))

        Nothing ->
            slider


brushEnd : Slider -> Slider
brushEnd { length, position } =
    Slider Nothing length position



-- update


type Msg
    = BrushStarted Point2D
    | BrushMoved Point2D
    | BrushEnded


type alias Point2D =
    { x : Float
    , y : Float
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg slider =
    ( case msg of
        BrushStarted point ->
            slider |> brushStart point

        BrushMoved point ->
            slider |> brushMove point

        BrushEnded ->
            slider |> brushEnd
    , Cmd.none
    )



-- events


subscriptions : Model -> Sub Msg
subscriptions slider =
    if slider |> isBrushing then
        subBrushing

    else
        Sub.none


subBrushing : Sub Msg
subBrushing =
    Sub.batch
        [ Browser.Events.onMouseMove (Decode.map BrushMoved decodeMousePosition)
        , Browser.Events.onMouseUp (Decode.succeed BrushEnded)
        , Browser.Events.onVisibilityChange (always BrushEnded)
        ]


decodeMousePosition : Decoder Point2D
decodeMousePosition =
    Decode.map2 Point2D
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)



-- view


view : Model -> Browser.Document Msg
view slider =
    Browser.Document
        "Slider"
        [ Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "../css/style.css"
            ]
            []
        , Svg.svg
            [ Svg.Attributes.width "500"
            , Svg.Attributes.height "500"
            ]
            [ Svg.g
                [ Svg.Attributes.transform "translate(100, 450) "
                ]
                [ viewSlider BrushStarted slider
                ]
            ]
        ]


viewSlider : (Point2D -> msg) -> Slider -> Svg msg
viewSlider tagger slider =
    Svg.g
        [ Svg.Attributes.class <| "slider" ++ bool " active" "" (slider |> isBrushing)
        ]
        [ Svg.line
            [ Svg.Attributes.x1 "0"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 <| String.fromFloat slider.length
            , Svg.Attributes.y2 "0"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.cx <| String.fromFloat slider.position
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "5"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.class "thumb"
            , Svg.Attributes.cx <| String.fromFloat slider.position
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "15"
            , Svg.Events.preventDefaultOn "mousedown" (decodeMousePosition |> Decode.map (\point -> ( tagger point, True )))
            ]
            []
        ]



-- helpers


bool : a -> a -> Bool -> a
bool t f x =
    if x then
        t

    else
        f
