module Render exposing (meshFigure, meshIcon)

import Dict
import Geometry exposing (..)
import Mesh exposing (Mesh, faceNormal, faceToPolygon)
import Svg exposing (Svg)
import Svg.Attributes


meshFigure : Vector -> (Int -> String) -> Mesh -> Svg a
meshFigure lightDirection faceClass { vertices, faces } =
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


meshIcon : Mesh -> Svg a
meshIcon { vertices, faces } =
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
