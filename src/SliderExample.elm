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
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { slider : Slider
    , brushing : Maybe Brush
    }


initialModel : Model
initialModel =
    { slider = Slider.init 300 0.5
    , brushing = Nothing
    }



-- update


type Msg
    = BrushStarted Point2D
    | BrushMoved Point2D
    | BrushEnded


type alias Brush =
    { from : Point2D
    , to : Point2D
    }


type alias Point2D =
    { x : Float
    , y : Float
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        BrushStarted point ->
            { model | brushing = Just { from = point, to = point } }

        BrushMoved point ->
            case model.brushing of
                Just { from } ->
                    { model | brushing = Just { from = from, to = point } }

                Nothing ->
                    model

        BrushEnded ->
            case model.brushing of
                Just brush ->
                    { slider = model.slider |> Slider.applyBrush brush
                    , brushing = Nothing
                    }

                Nothing ->
                    model
    , Cmd.none
    )



-- events


subscriptions : Model -> Sub Msg
subscriptions { brushing } =
    case brushing of
        Just _ ->
            subBrushing

        Nothing ->
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
view { brushing, slider } =
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
                [ Slider.view BrushStarted brushing slider
                ]
            ]
        ]
