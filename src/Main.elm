module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes


main : Html a
main =
    view



--


type alias Point =
    { x : Float
    , y : Float
    , z : Float
    }


type alias Face =
    List Int


type alias Mesh =
    { vertices : Dict Int Point
    , faces : Dict Int Face
    }



--


type alias Vector =
    { x : Float
    , y : Float
    , z : Float
    }


vectorZero : Vector
vectorZero =
    Vector 0 0 0


vectorScale : Float -> Vector -> Vector
vectorScale factor { x, y, z } =
    { x = x * factor
    , y = y * factor
    , z = z * factor
    }


vectorLength : Vector -> Float
vectorLength { x, y, z } =
    sqrt (x * x + y * y + z * z)


vectorNormalize : Vector -> Vector
vectorNormalize ({ x, y, z } as v) =
    let
        len =
            vectorLength v
    in
    { x = x / len
    , y = y / len
    , z = z / len
    }


vectorCross : Vector -> Vector -> Vector
vectorCross a b =
    { x = a.y * b.z - a.z * b.y
    , y = a.z * b.x - a.x * b.z
    , z = a.x * b.y - a.y * b.x
    }


vectorAdd : Vector -> Vector -> Vector
vectorAdd a b =
    { x = a.x + b.x
    , y = a.y + b.y
    , z = a.z + b.z
    }


vectorSubtract : Vector -> Vector -> Vector
vectorSubtract a b =
    { x = a.x - b.x
    , y = a.y - b.y
    , z = a.z - b.z
    }



--


type alias Matrix =
    { a : Vector
    , b : Vector
    , c : Vector
    , t : Vector
    }


matrixIdentity : Matrix
matrixIdentity =
    { a = Vector 1 0 0
    , b = Vector 0 1 0
    , c = Vector 0 0 1
    , t = Vector 0 0 0
    }


matrixTransform : Matrix -> Vector -> Vector
matrixTransform { a, b, c, t } { x, y, z } =
    { x = x * a.x + y * b.x + z * c.x + t.x
    , y = x * a.y + y * b.y + z * c.y + t.y
    , z = x * a.z + y * b.z + z * c.z + t.z
    }


matrixLookAt : Point -> Point -> Matrix
matrixLookAt from to =
    let
        zaxis =
            vectorSubtract from to |> vectorNormalize

        xaxis =
            vectorCross (Vector 0 1 0) zaxis |> vectorNormalize

        yaxis =
            vectorCross zaxis xaxis
    in
    { a = Vector xaxis.x yaxis.x zaxis.x
    , b = Vector xaxis.y yaxis.y zaxis.y
    , c = Vector xaxis.z yaxis.z zaxis.z
    , t = to
    }



-- meshes


axes =
    { vertices =
        [ ( 1, Point 0 0 0 )
        , ( 2, Point 1 0 0 )
        , ( 3, Point 0 1 0 )
        , ( 4, Point 0 0 1 )
        ]
            |> Dict.fromList
            |> Dict.map (always (vectorScale 100))
    , faces =
        [ ( 1, [ 1, 2, 3 ] )
        , ( 2, [ 1, 3, 4 ] )
        , ( 3, [ 1, 2, 4 ] )
        ]
            |> Dict.fromList
    }


tetrahedron =
    let
        centroid =
            Point 0.5 (sqrt (2 / 3) / 4) (sqrt 0.75 / 3)
    in
    { vertices =
        [ ( 1, Point 0 0 0 )
        , ( 2, Point 1 0 0 )
        , ( 3, Point 0.5 0 (sqrt 0.75) )
        , ( 4, Point 0.5 (sqrt (2 / 3)) (sqrt 0.75 / 3) )
        ]
            |> Dict.fromList
            |> Dict.map (\_ v -> vectorSubtract v centroid |> vectorScale 200)
    , faces =
        [ ( 4, [ 3, 2, 1 ] )
        , ( 2, [ 4, 2, 3 ] )
        , ( 3, [ 4, 3, 1 ] )
        , ( 1, [ 4, 1, 2 ] )
        ]
            |> Dict.fromList
    }


cube =
    let
        centroid =
            Point 0.5 0.5 0.5
    in
    { vertices =
        [ ( 1, Point 0 0 0 )
        , ( 2, Point 0 0 1 )
        , ( 3, Point 0 1 0 )
        , ( 4, Point 0 1 1 )
        , ( 5, Point 1 0 0 )
        , ( 6, Point 1 0 1 )
        , ( 7, Point 1 1 0 )
        , ( 8, Point 1 1 1 )
        ]
            |> Dict.fromList
            |> Dict.map (\_ v -> vectorSubtract v centroid |> vectorScale 150)
    , faces =
        [ ( 1, [ 1, 5, 7, 3 ] )
        , ( 2, [ 1, 3, 4, 2 ] )
        , ( 3, [ 1, 2, 6, 5 ] )
        , ( 4, [ 2, 4, 8, 6 ] )
        , ( 5, [ 5, 6, 8, 7 ] )
        , ( 6, [ 3, 7, 8, 4 ] )
        ]
            |> Dict.fromList
    }



-- view


view : Html a
view =
    let
        cam =
            matrixLookAt (Vector 2 3 5) vectorZero |> Debug.log "cam"

        mesh =
            { cube | vertices = cube.vertices |> Dict.map (always (matrixTransform cam)) }
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
                [ viewMesh mesh
                ]
            ]
        ]


viewMesh : Mesh -> Svg a
viewMesh { vertices, faces } =
    Svg.g
        []
        (faces |> Dict.values |> List.map (viewFace vertices))


viewFace : Dict Int Point -> Face -> Svg a
viewFace vertices face =
    let
        points =
            face |> List.map (\v -> Dict.get v vertices |> Maybe.withDefault vectorZero)
    in
    Svg.polygon
        [ Svg.Attributes.points (points |> pointsToString)
        ]
        []


pointsToString : List Point -> String
pointsToString =
    List.foldl
        (\p result -> result ++ " " ++ pointToString p)
        ""


pointToString : Point -> String
pointToString { x, y } =
    String.fromFloat x ++ "," ++ String.fromFloat y
