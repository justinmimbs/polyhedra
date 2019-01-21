module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Geometry exposing (..)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Mesh exposing (Mesh, SuperMesh, faceNormal, faceToPolygon, reify)
import Polyhedron exposing (bitruncate, cube, dodecahedron, icosahedron, octahedron, tetrahedron, truncate)
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


type alias Model =
    { seedFaces : Set Int
    , truncation : SuperMesh
    , bitruncation : SuperMesh
    , slider : Slider
    , brushing : Maybe Brush
    }


init : Mesh -> Model
init polyhedron =
    { seedFaces = polyhedron.faces |> Dict.keys |> Set.fromList
    , truncation = truncate polyhedron
    , bitruncation = bitruncate polyhedron
    , slider = Slider.init 260 0
    , brushing = Nothing
    }


type alias Brush =
    { from : Point2D
    , to : Point2D
    }


type alias Point2D =
    { x : Float
    , y : Float
    }



-- update


type Msg
    = BrushStarted Point2D
    | BrushMoved Point2D
    | BrushEnded


update : Msg -> Model -> Model
update msg model =
    case msg of
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
                    { model
                        | slider = model.slider |> Slider.applyBrush brush
                        , brushing = Nothing
                    }

                Nothing ->
                    model



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
view =
    let
        cameraMatrix : Matrix
        cameraMatrix =
            matrixLookAt (Vector 3 3 5) vectorZero

        lightDirection : Vector
        lightDirection =
            Vector -3 -6 1 |> matrixMultiplyVector cameraMatrix |> vectorNormalize
    in
    \{ seedFaces, truncation, bitruncation, slider, brushing } ->
        let
            faceClass : Int -> String
            faceClass f =
                if Set.member f seedFaces then
                    "face a"

                else
                    "face b"

            t =
                Slider.value brushing slider * 2

            mesh =
                if t <= 1.0 then
                    reify t truncation

                else
                    reify (t - 1.0) bitruncation

            radius =
                mesh.vertices
                    |> Dict.foldl (\_ p -> max (vectorLengthSquared p)) 0
                    |> sqrt

            matrix =
                cameraMatrix |> matrixScale (160 / radius)

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
                    [ Svg.Attributes.transform "scale(1, -1) translate(200, -200) "
                    ]
                    [ viewMesh lightDirection faceClass meshTransformed
                    ]
                , Svg.g
                    [ Svg.Attributes.transform "translate(70, 440) "
                    ]
                    [ Slider.view BrushStarted brushing slider
                    ]
                ]
            ]


viewMesh : Vector -> (Int -> String) -> Mesh -> Svg a
viewMesh lightDirection faceClass { vertices, faces } =
    let
        ( backFaces, frontFaces, lumaFaces ) =
            faces
                |> Dict.foldl
                    (\f face ( backs, fronts, luma ) ->
                        let
                            polygon =
                                face |> faceToPolygon vertices

                            faceView =
                                polygon |> viewPolygon [] (faceClass f)
                        in
                        if polygon |> polygonIsClockwise then
                            let
                                -- range [-1, 1], where -1 is facing toward light, 0 is perpendicular, and 1 is facing away
                                alpha =
                                    vectorDot lightDirection (faceNormal polygon)

                                lumaView =
                                    if alpha <= 0 then
                                        viewPolygon
                                            [ Svg.Attributes.opacity <| String.fromFloat ((0 - alpha) ^ 2) ]
                                            "light"
                                            polygon

                                    else
                                        viewPolygon
                                            [ Svg.Attributes.opacity <| String.fromFloat (alpha ^ 2) ]
                                            "dark"
                                            polygon
                            in
                            ( backs, faceView :: fronts, lumaView :: luma )

                        else
                            ( faceView :: backs, fronts, luma )
                    )
                    ( [], [], [] )
    in
    Svg.g
        []
        (backFaces ++ frontFaces ++ lumaFaces)


viewPolygon : List (Svg.Attribute a) -> String -> Polygon -> Svg a
viewPolygon attributes class points =
    Svg.polygon
        (attributes
            ++ [ Svg.Attributes.class class
               , Svg.Attributes.points (points |> pointsToString)
               ]
        )
        []


pointsToString : List Point -> String
pointsToString =
    List.foldl
        (\p result -> result ++ " " ++ pointToString p)
        ""


pointToString : Point -> String
pointToString { x, y } =
    String.fromFloat x ++ "," ++ String.fromFloat y
