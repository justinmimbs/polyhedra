module MenuExample exposing (main)

import Dict exposing (Dict)
import Geometry exposing (..)
import Html exposing (Html)
import Html.Attributes
import Mesh exposing (Mesh, faceNormal, faceToPolygon)
import Polyhedron exposing (cube, dodecahedron, icosahedron, octahedron, tetrahedron)
import Svg exposing (Svg)
import Svg.Attributes


main : Html a
main =
    view quaternionIdentity


view : Quaternion -> Html a
view =
    let
        cameraMatrix : Matrix
        cameraMatrix =
            matrixLookAt (Vector 3 3 5) vectorZero
    in
    \orientation ->
        let
            rotationMatrix =
                orientation |> quaternionToMatrix

            matrix =
                matrixMultiply cameraMatrix rotationMatrix |> matrixScale 26

            meshes =
                [ tetrahedron, cube, octahedron, icosahedron, dodecahedron ]
        in
        Html.div
            []
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
                    (List.indexedMap
                        (\i mesh ->
                            Svg.g
                                [ Svg.Attributes.transform <| "translate(" ++ String.fromInt (-120 + (i * 60)) ++ ", 0) "
                                ]
                                [ viewMeshIcon
                                    { mesh | vertices = mesh.vertices |> Dict.map (\_ -> matrixMultiplyVector matrix) }
                                ]
                        )
                        meshes
                    )
                ]
            ]


viewMeshIcon : Mesh -> Svg a
viewMeshIcon { vertices, faces } =
    let
        ( backFaces, frontFaces ) =
            faces
                |> Dict.foldl
                    (\f face ( backs, fronts ) ->
                        let
                            polygon =
                                face |> faceToPolygon vertices

                            faceView =
                                polygon |> viewPolygon [] "face"
                        in
                        if polygon |> polygonIsClockwise then
                            ( backs, faceView :: fronts )

                        else
                            ( faceView :: backs, fronts )
                    )
                    ( [], [] )
    in
    Svg.g
        [ Svg.Attributes.class "icon"
        ]
        (backFaces ++ frontFaces)


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
