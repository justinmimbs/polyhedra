module RotationExample exposing (main)

import Browser
import Brush exposing (Brush, Point2D)
import Dict exposing (Dict)
import Geometry exposing (..)
import Html exposing (Html)
import Html.Attributes
import Mesh exposing (Mesh, SuperMesh, faceNormal, faceToPolygon, reify)
import Polyhedron exposing (cube)
import Render
import Set exposing (Set)
import Slider exposing (Slider)
import Svg exposing (Svg)
import Svg.Attributes


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initialModel, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { orientation : Quaternion
    , brushing : Maybe Brush
    }


initialModel : Model
initialModel =
    { orientation =
        quaternionMultiply
            (quaternionFromAxisAngle (Vector 0 1 0) (pi / 5.5))
            (quaternionFromAxisAngle (Vector 1 0 0) (-pi / 6.5))
    , brushing =
        Nothing
    }



-- update


type Msg
    = BrushStarted Point2D
    | BrushMoved Point2D
    | BrushEnded


update : Msg -> Model -> Model
update msg ({ orientation, brushing } as model) =
    case msg of
        BrushStarted point ->
            { model | brushing = Just (Brush.init point) }

        BrushMoved point ->
            { model | brushing = model.brushing |> Maybe.map (Brush.update point) }

        BrushEnded ->
            { orientation = orientation |> orientationWithBrushing brushing
            , brushing = Nothing
            }


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


orientationWithBrushing : Maybe Brush -> Quaternion -> Quaternion
orientationWithBrushing brushing orientation =
    case brushing of
        Just brush ->
            quaternionMultiply orientation (rotationFromBrush brush)

        Nothing ->
            orientation



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
view { orientation, brushing } =
    let
        rotationMatrix =
            orientation
                |> orientationWithBrushing brushing
                |> quaternionToMatrix

        matrix =
            rotationMatrix |> matrixScale 160

        meshTransformed =
            { cube | vertices = cube.vertices |> Dict.map (\_ -> matrixMultiplyVector matrix) }
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
                [ Render.meshFigure lightDirection (always "face a") meshTransformed
                , Svg.circle
                    [ Svg.Attributes.cx "0"
                    , Svg.Attributes.cy "0"
                    , Svg.Attributes.r "160"
                    , Svg.Attributes.opacity "0"
                    , Brush.onStart BrushStarted
                    ]
                    []
                ]
            ]
        ]


lightDirection : Vector
lightDirection =
    Vector -2 -3 -2 |> vectorNormalize
