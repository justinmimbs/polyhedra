module Main exposing (main)

import Browser
import Brush exposing (Brush, Point2D)
import Dict exposing (Dict)
import Geometry exposing (..)
import Html exposing (Html)
import Html.Attributes
import Mesh exposing (Mesh, SuperMesh, reify)
import Polyhedron exposing (bitruncate, icosahedron, truncate)
import Render
import Set exposing (Set)
import Slider exposing (Slider)
import Svg exposing (Svg)
import Svg.Attributes


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( init icosahedron, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }


init : Mesh -> Model
init polyhedron =
    { seedFaces = polyhedron.faces |> Dict.keys |> Set.fromList
    , truncation = truncate polyhedron
    , bitruncation = bitruncate polyhedron
    , orientation =
        quaternionMultiply
            (quaternionFromAxisAngle (Vector 0 1 0) (pi / 5.5))
            (quaternionFromAxisAngle (Vector 1 0 0) (-pi / 6.5))
    , slider = Slider.init 260 0
    , brushing = Nothing
    }


type alias Model =
    { seedFaces : Set Int
    , truncation : SuperMesh
    , bitruncation : SuperMesh
    , orientation : Quaternion
    , slider : Slider
    , brushing : Maybe ( BrushTarget, Brush )
    }


type BrushTarget
    = SliderPosition
    | ObjectRotation



-- update


type Msg
    = BrushStarted BrushTarget Point2D
    | BrushMoved Point2D
    | BrushEnded


update : Msg -> Model -> Model
update msg ({ orientation, slider, brushing } as model) =
    case msg of
        BrushStarted brushTarget point ->
            { model | brushing = Just ( brushTarget, Brush.init point ) }

        BrushMoved point ->
            { model | brushing = brushing |> Maybe.map (Tuple.mapSecond (Brush.update point)) }

        BrushEnded ->
            case brushing of
                Just ( SliderPosition, brush ) ->
                    { model
                        | slider = slider |> Slider.applyBrush brush
                        , brushing = Nothing
                    }

                Just ( ObjectRotation, brush ) ->
                    { model
                        | orientation = quaternionMultiply orientation (rotationFromBrush brush)
                        , brushing = Nothing
                    }

                Nothing ->
                    model


rotationFromBrush : Brush -> Quaternion
rotationFromBrush { from, to } =
    let
        dx =
            to.x - from.x

        dy =
            to.y - from.y

        axis =
            Vector -dy -dx 0 |> vectorNormalize

        angle =
            (sqrt (dx * dx + dy * dy) / 320) * pi
    in
    quaternionFromAxisAngle axis angle



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
view { seedFaces, truncation, bitruncation, orientation, slider, brushing } =
    let
        sliderBrushing =
            case brushing of
                Just ( SliderPosition, brush ) ->
                    Just brush

                _ ->
                    Nothing

        t =
            Slider.value sliderBrushing slider * 2

        mesh =
            if t <= 1.0 then
                reify t truncation

            else
                reify (t - 1.0) bitruncation

        radius =
            mesh.vertices
                |> Dict.foldl (\_ p -> max (vectorLengthSquared p)) 0
                |> sqrt

        rotationMatrix =
            (case brushing of
                Just ( ObjectRotation, brush ) ->
                    quaternionMultiply orientation (rotationFromBrush brush)

                _ ->
                    orientation
            )
                |> quaternionToMatrix

        matrix =
            rotationMatrix |> matrixScale (160 / radius)

        meshTransformed =
            { mesh | vertices = mesh.vertices |> Dict.map (\_ -> matrixMultiplyVector matrix) }
    in
    Browser.Document
        "Polyhedra"
        [ Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "../css/style.css"
            ]
            []
        , Svg.svg
            [ Svg.Attributes.width "400"
            , Svg.Attributes.height "500"
            ]
            [ Svg.g
                [ Svg.Attributes.transform "translate(200, 200) scale(1 -1)"
                ]
                [ Render.meshFigure lightDirection (faceClass seedFaces) meshTransformed
                , Svg.circle
                    [ Svg.Attributes.cx "0"
                    , Svg.Attributes.cy "0"
                    , Svg.Attributes.r "160"
                    , Svg.Attributes.opacity "0"
                    , Brush.onStart (BrushStarted ObjectRotation)
                    ]
                    []
                ]
            , Svg.g
                [ Svg.Attributes.transform "translate(70, 440) "
                ]
                [ Slider.view (BrushStarted SliderPosition) sliderBrushing slider
                ]
            ]
        ]


lightDirection : Vector
lightDirection =
    Vector -2 -3 -2 |> vectorNormalize


faceClass : Set Int -> Int -> String
faceClass faces f =
    if Set.member f faces then
        "face a"

    else
        "face b"
