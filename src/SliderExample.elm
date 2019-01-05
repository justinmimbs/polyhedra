module SliderExample exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Slider exposing (Slider)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( Slider.init 300 0.5, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    Slider



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
            slider |> Slider.brushStart point

        BrushMoved point ->
            slider |> Slider.brushMove point

        BrushEnded ->
            slider |> Slider.brushEnd
    , Cmd.none
    )



-- events


subscriptions : Model -> Sub Msg
subscriptions slider =
    if slider |> Slider.isBrushing then
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
                [ Slider.view BrushStarted slider
                ]
            ]
        ]
