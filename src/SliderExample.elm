module SliderExample exposing (main)

import Browser
import Brush exposing (Brush, Point2D)
import Html exposing (Html)
import Html.Attributes
import Slider exposing (Slider)
import Svg exposing (Svg)
import Svg.Attributes


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        BrushStarted point ->
            { model | brushing = Just (Brush.init point) }

        BrushMoved point ->
            { model | brushing = model.brushing |> Maybe.map (Brush.update point) }

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
subscriptions =
    let
        brushSubscriptions =
            Brush.subscriptions BrushMoved BrushEnded
    in
    \{ brushing } ->
        case brushing of
            Just _ ->
                brushSubscriptions

            Nothing ->
                Sub.none



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
            (case brushing of
                Just _ ->
                    [ Brush.touchMove BrushMoved
                    , Brush.touchEnd BrushEnded
                    ]

                Nothing ->
                    []
            )
            [ Svg.svg
                [ Svg.Attributes.x "50%"
                , Svg.Attributes.y "50%"
                ]
                [ Svg.g
                    [ Svg.Attributes.transform "translate(-150, 0)"
                    ]
                    [ Slider.view BrushStarted brushing slider
                    ]
                , case brushing of
                    Just brush ->
                        Svg.g
                            [ Svg.Attributes.transform "translate(0, -70) "
                            ]
                            [ Svg.text_ [] [ Svg.text ([ brush.to.x, brush.to.y ] |> List.map String.fromFloat |> String.join " , ") ]
                            ]

                    Nothing ->
                        Svg.text ""
                ]
            ]
        ]
