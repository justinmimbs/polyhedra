module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes


main : Html a
main =
    view



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



--


type alias Point =
    { x : Float
    , y : Float
    , z : Float
    }


interpolate : Float -> Point -> Point -> Point
interpolate t a b =
    { x = a.x + ((b.x - a.x) * t)
    , y = a.y + ((b.y - a.y) * t)
    , z = a.z + ((b.z - a.z) * t)
    }


midpoint : Point -> Point -> Point
midpoint =
    interpolate 0.5



--


type alias Polygon =
    List Point


{-| Based on the shoelace formula for area of a simple polygon (where `area =
abs (area2 / 2)`).

The value of area2 will be positive when points are ordered clockwise, zero
when collinear, and negative when counterclockwise.

-}
polygonIsClockwise : Polygon -> Bool
polygonIsClockwise points =
    let
        area2 =
            foldCycle
                (\a b result -> (b.x - a.x) * (b.y + a.y) + result)
                0
                points
    in
    0 < area2


{-| Assume regular polygon and take arithmetic mean of points.
-}
polygonCenter : Polygon -> Point
polygonCenter points =
    case List.length points of
        0 ->
            vectorZero

        n ->
            points |> List.foldl vectorAdd vectorZero |> vectorScale (1 / toFloat n)



--


type alias Face =
    List Int


faceToPolygon : Dict Int Point -> Face -> Polygon
faceToPolygon vertices face =
    List.map
        (lookup vectorZero vertices)
        face



--


type alias Mesh =
    { vertices : Dict Int Point
    , faces : Dict Int Face
    }


mergeCoincidentVertices : Mesh -> Mesh
mergeCoincidentVertices ({ vertices, faces } as mesh) =
    let
        points : Dict ( Float, Float, Float ) Int
        points =
            vertices |> Dict.foldl (\v { x, y, z } -> Dict.insert ( x, y, z ) v) Dict.empty
    in
    if Dict.size vertices == Dict.size points then
        mesh

    else
        let
            vertexToMerged : Dict Int Int
            vertexToMerged =
                vertices |> Dict.map (\v { x, y, z } -> lookup 0 points ( x, y, z ))
        in
        { vertices =
            points |> Dict.foldl (\( x, y, z ) u -> Dict.insert u (Point x y z)) Dict.empty
        , faces =
            faces
                |> Dict.map
                    (\_ face ->
                        face
                            |> List.map (lookup 0 vertexToMerged)
                            -- remove consecutive duplicates
                            |> foldCycle
                                (\a b result ->
                                    if a == b then
                                        result

                                    else
                                        a :: result
                                )
                                []
                            |> List.reverse
                    )
        }



-- super


type alias SuperVertex =
    ( Int, Int )


type alias SuperFace =
    List SuperVertex


type alias SuperMesh =
    { vertices0 : Dict SuperVertex Point
    , vertices1 : Dict SuperVertex Point
    , faces : Dict Int SuperFace
    }


indexVertices : Dict SuperVertex Point -> Dict Int SuperFace -> Mesh
indexVertices vertices faces =
    let
        vuToIndex =
            vertices |> Dict.keys |> List.indexedMap (\i vu -> ( vu, i )) |> Dict.fromList
    in
    { vertices =
        vertices |> Dict.foldl (\vu -> Dict.insert (lookup 0 vuToIndex vu)) Dict.empty
    , faces =
        faces |> Dict.map (\_ -> List.map (lookup 0 vuToIndex))
    }


reify : Float -> SuperMesh -> Mesh
reify t { vertices0, vertices1, faces } =
    let
        interpolatedVertices =
            vertices0
                |> Dict.map
                    (\vu p0 ->
                        let
                            p1 =
                                vertices1 |> Dict.get vu |> Maybe.withDefault vectorZero
                        in
                        interpolate t p0 p1
                    )

        mesh =
            indexVertices interpolatedVertices faces
    in
    if t == 1 || t == 0 then
        mesh |> mergeCoincidentVertices

    else
        mesh



-- truncation


truncate : Mesh -> SuperMesh
truncate { vertices, faces } =
    let
        newFaceBaseIndex : Int
        newFaceBaseIndex =
            (faces |> Dict.keys |> List.maximum |> Maybe.withDefault 0) + 1

        -- existing faces truncated
        truncatedFaces : Dict Int SuperFace
        truncatedFaces =
            faces
                |> Dict.map
                    (\_ ->
                        foldCycle
                            (\v u result ->
                                ( u, v ) :: ( v, u ) :: result
                            )
                            []
                            >> List.reverse
                    )

        -- new faces at truncated vertices
        truncatedVertices : Dict Int SuperFace
        truncatedVertices =
            truncatedFaces
                -- find all edges with the same source vertex, indexing by source vertex
                -- for each source vertex, these are the edges that make up the new face
                |> Dict.foldl
                    (\_ face vertexEdges ->
                        foldCycle
                            (\(( v1, _ ) as vu1) (( v2, _ ) as vu2) result ->
                                -- same source vertex
                                if v1 == v2 then
                                    Dict.update
                                        v1
                                        -- reverse edge order
                                        (Maybe.withDefault [] >> (::) ( vu2, vu1 ) >> Just)
                                        result

                                else
                                    result
                            )
                            vertexEdges
                            face
                    )
                    Dict.empty
                |> Dict.values
                -- use edges to make a path for each new face
                |> List.indexedMap
                    (\i edges ->
                        ( newFaceBaseIndex + i
                        , edges |> toTruncatedVertex
                        )
                    )
                |> Dict.fromList

        newVertices : List SuperVertex
        newVertices =
            truncatedVertices |> Dict.values |> List.concat
    in
    { vertices0 =
        -- original points
        newVertices
            |> List.map
                (\(( v, _ ) as vu) ->
                    ( vu
                    , Dict.get v vertices |> Maybe.withDefault vectorZero
                    )
                )
            |> Dict.fromList
    , vertices1 =
        -- rectified points
        newVertices
            |> List.map
                (\(( v, u ) as vu) ->
                    ( vu
                    , midpoint
                        (Dict.get v vertices |> Maybe.withDefault vectorZero)
                        (Dict.get u vertices |> Maybe.withDefault vectorZero)
                    )
                )
            |> Dict.fromList
    , faces =
        Dict.union truncatedFaces truncatedVertices
    }


{-| Given an unordered list of directed edges, create a path starting from an
arbitrary edge.
-}
toTruncatedVertex : List ( a, a ) -> List a
toTruncatedVertex edges =
    case edges of
        ( x, y ) :: rest ->
            simplePathHelp x rest y [ y, x ]

        [] ->
            []


{-| Does every supervertex `(v, u)` have the same source vertex `v`?
-}
isTruncatedVertex : SuperFace -> Bool
isTruncatedVertex face =
    case face of
        ( v1, _ ) :: rest ->
            rest |> List.all (\( vn, _ ) -> vn == v1)

        _ ->
            False


bitruncate : Mesh -> SuperMesh
bitruncate polyhedron =
    let
        truncated : SuperMesh
        truncated =
            truncate polyhedron

        rectified : Mesh
        rectified =
            reify 1 truncated

        collapsing : Set Int
        collapsing =
            truncated.faces
                |> Dict.foldl
                    (\f superface col ->
                        if superface |> isTruncatedVertex then
                            col

                        else
                            col |> Set.insert f
                    )
                    Set.empty

        -- each vertex is incident to exactly two collapsing faces
        vToPair : Dict Int ( Int, Int )
        vToPair =
            collapsing
                -- indexFacesByVertex
                |> Set.foldl
                    (\f result ->
                        List.foldl
                            (\v -> Dict.update v (Maybe.withDefault [] >> (::) f >> Just))
                            result
                            (lookup [] rectified.faces f)
                    )
                    Dict.empty
                |> Dict.map
                    -- pairFromList
                    (\_ list ->
                        case list of
                            [ f1, f2 ] ->
                                orderPair ( f1, f2 )

                            _ ->
                                ( 0, 0 )
                    )

        -- rename vertices
        updatedFaces : Dict Int SuperFace
        updatedFaces =
            rectified.faces
                |> Dict.map
                    (\f face ->
                        let
                            superface =
                                face |> List.map (lookup ( 0, 0 ) vToPair)
                        in
                        if collapsing |> Set.member f then
                            -- collapsing face
                            superface
                                |> List.map
                                    (\( v, u ) ->
                                        -- order each pair with collapsing face first
                                        if v == f then
                                            ( v, u )

                                        else
                                            ( u, v )
                                    )

                        else
                            -- expanding face
                            superface
                                |> foldCycle
                                    (\( a, b ) ( c, d ) expandedFace ->
                                        if a == c || a == d then
                                            ( a, b ) :: ( b, a ) :: expandedFace

                                        else
                                            ( b, a ) :: ( a, b ) :: expandedFace
                                    )
                                    []
                                |> List.reverse
                    )

        pairToV : Dict ( Int, Int ) Int
        pairToV =
            vToPair |> Dict.foldl (\v vu -> Dict.insert vu v) Dict.empty

        faceToCenter : Dict Int Point
        faceToCenter =
            rectified.faces |> Dict.map (\_ -> faceToPolygon rectified.vertices >> polygonCenter)

        newVertices : List SuperVertex
        newVertices =
            updatedFaces |> Dict.filter (\f _ -> Set.member f collapsing) |> Dict.values |> List.concat
    in
    { vertices0 =
        -- rectified points
        newVertices
            |> List.map
                (\vu ->
                    ( vu
                    , orderPair vu |> lookup 0 pairToV |> lookup vectorZero rectified.vertices
                    )
                )
            |> Dict.fromList
    , vertices1 =
        -- dual points
        newVertices
            |> List.map
                (\(( f, _ ) as vu) ->
                    ( vu
                    , lookup vectorZero faceToCenter f
                    )
                )
            |> Dict.fromList
    , faces = updatedFaces
    }



-- pair


areSymmetricPairs : ( a, a ) -> ( a, a ) -> Bool
areSymmetricPairs ( a, b ) ( c, d ) =
    a == d && b == c


isOrderedPair : ( comparable, comparable ) -> Bool
isOrderedPair ( a, b ) =
    a < b


orderPair : ( comparable, comparable ) -> ( comparable, comparable )
orderPair (( a, b ) as ab) =
    if b < a then
        ( b, a )

    else
        ab



-- graph


simplePath : a -> List ( a, a ) -> List a
simplePath start edges =
    simplePathHelp start edges start [ start ]


simplePathHelp : a -> List ( a, a ) -> a -> List a -> List a
simplePathHelp start edges prev path =
    case find (\( x, y ) -> prev == x && y /= start) edges of
        Just ( _, next ) ->
            simplePathHelp start edges next (next :: path)

        Nothing ->
            path |> List.reverse



-- mesh constants


tetrahedron : Mesh
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


cube : Mesh
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
            matrixLookAt (Vector 3 3 5) vectorZero

        ex =
            -- cube |> truncate |> reify (sqrt 2 / (1 + sqrt 2))
            cube |> bitruncate |> reify 0.2

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

        mesh =
            { ex | vertices = ex.vertices |> Dict.map (always (matrixTransform cam)) }
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
                [ viewMesh faceClass mesh
                ]
            ]
        ]


viewMesh : (Int -> String) -> Mesh -> Svg a
viewMesh faceClass { vertices, faces } =
    let
        ( frontFaces, backFaces ) =
            faces
                |> Dict.foldl
                    (\f face ( fronts, backs ) ->
                        let
                            polygon =
                                face |> faceToPolygon vertices

                            polygonView =
                                polygon |> viewPolygon (faceClass f)
                        in
                        if polygon |> polygonIsClockwise then
                            ( polygonView :: fronts, backs )

                        else
                            ( fronts, polygonView :: backs )
                    )
                    ( [], [] )
    in
    Svg.g
        []
        (backFaces ++ frontFaces)


viewPolygon : String -> Polygon -> Svg a
viewPolygon class points =
    Svg.polygon
        [ Svg.Attributes.class class
        , Svg.Attributes.points (points |> pointsToString)
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



-- List


{-| Left fold over each item with the next, and the last item with the first.
-}
foldCycle : (a -> a -> b -> b) -> b -> List a -> b
foldCycle f2 result list =
    case list of
        head :: _ ->
            foldCycleHelp head f2 result list

        [] ->
            result


foldCycleHelp : a -> (a -> a -> b -> b) -> b -> List a -> b
foldCycleHelp head f2 result list =
    case list of
        x :: ((y :: _) as rest) ->
            foldCycleHelp head f2 (f2 x y result) rest

        last :: [] ->
            f2 last head result

        [] ->
            result


find : (a -> Bool) -> List a -> Maybe a
find pred list =
    case list of
        x :: rest ->
            if pred x then
                Just x

            else
                find pred rest

        [] ->
            Nothing



-- Dict


lookup : v -> Dict comparable v -> comparable -> v
lookup default dict key =
    case Dict.get key dict of
        Just value ->
            value

        Nothing ->
            default
