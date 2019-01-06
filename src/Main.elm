module Main exposing (main)

import Dict exposing (Dict)
import Geometry exposing (..)
import Html exposing (Html)
import Html.Attributes
import Mesh exposing (Mesh, faceNormal, faceToPolygon, reify)
import Polyhedron exposing (bitruncate, cube, truncate)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes


main : Html a
main =
    view


view : Html a
view =
    let
        mesh =
            -- cube |> truncate |> reify (sqrt 2 / (1 + sqrt 2))
            cube |> bitruncate |> reify 0.2

        radius =
            mesh.vertices
                |> Dict.foldl (\_ p -> max (vectorLengthSquared p)) 0
                |> sqrt

        faceClass =
            let
                originalFaces =
                    cube.faces |> Dict.keys |> Set.fromList
            in
            \f ->
                if Set.member f originalFaces then
                    "color1"

                else
                    "color2"

        cameraMatrix =
            matrixLookAt (Vector 3 3 5) vectorZero
                |> matrixScale (140 / radius)

        lightDirection =
            Vector -3 -6 1 |> matrixMultiply cameraMatrix |> vectorNormalize

        meshTransformed =
            { mesh | vertices = mesh.vertices |> Dict.map (\_ -> matrixMultiply cameraMatrix) }
    in
    Html.div
        []
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
                [ Svg.Attributes.transform "scale(1, -1) translate(250, -250) "
                ]
                [ viewMesh lightDirection faceClass meshTransformed
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
