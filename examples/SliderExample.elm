module SliderExample exposing (main)

import Browser
import Browser.Events
import Brush exposing (Brush)
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
    = BrushStarted Brush
    | BrushMoved Brush
    | BrushEnded


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        BrushStarted brush ->
            { model | brushing = Just brush }

        BrushMoved brush ->
            { model | brushing = model.brushing |> Maybe.map (always brush) }

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
    if brushing /= Nothing then
        Browser.Events.onVisibilityChange (always BrushEnded)

    else
        Sub.none



-- view


view : Model -> Browser.Document Msg
view { brushing, slider } =
    Browser.Document
        "Slider"
        [ Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "slider.css"
            ]
            []
        , Svg.svg
            (case brushing of
                Just brush ->
                    Brush.onBrush BrushMoved BrushEnded brush

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
                ]
            ]
        ]
