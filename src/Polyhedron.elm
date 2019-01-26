module Polyhedron exposing
    ( truncate, bitruncate
    , tetrahedron, cube, octahedron, icosahedron, dodecahedron
    )

{-|

@docs truncate, bitruncate
@docs tetrahedron, cube, octahedron, icosahedron, dodecahedron

-}

import Dict exposing (Dict)
import Extra.Dict as Dict
import Extra.List as List
import Geometry exposing (..)
import Mesh exposing (Face, Mesh, SuperFace, SuperMesh, SuperVertex, faceToPolygon, reify)
import Set exposing (Set)


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
                        List.foldCycle
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
                        List.foldCycle
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
                            (Dict.lookup [] rectified.faces f)
                    )
                    Dict.empty
                |> Dict.map
                    -- pairFromList
                    (\_ list ->
                        case list of
                            [ f1, f2 ] ->
                                orderedPair f1 f2

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
                                face |> List.map (Dict.lookup ( 0, 0 ) vToPair)
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
                                |> List.foldCycle
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
                (\(( v, u ) as vu) ->
                    ( vu
                    , orderedPair v u |> Dict.lookup 0 pairToV |> Dict.lookup vectorZero rectified.vertices
                    )
                )
            |> Dict.fromList
    , vertices1 =
        -- dual points
        newVertices
            |> List.map
                (\(( f, _ ) as vu) ->
                    ( vu
                    , Dict.lookup vectorZero faceToCenter f
                    )
                )
            |> Dict.fromList
    , faces = updatedFaces
    }



-- pair


orderedPair : comparable -> comparable -> ( comparable, comparable )
orderedPair a b =
    if a <= b then
        ( a, b )

    else
        ( b, a )



-- graph


simplePath : a -> List ( a, a ) -> List a
simplePath start edges =
    simplePathHelp start edges start [ start ]


simplePathHelp : a -> List ( a, a ) -> a -> List a -> List a
simplePathHelp start edges prev path =
    case List.find (\( x, y ) -> prev == x && y /= start) edges of
        Just ( _, next ) ->
            simplePathHelp start edges next (next :: path)

        Nothing ->
            path |> List.reverse



-- mesh definitions


tetrahedron : Mesh
tetrahedron =
    { vertices =
        [ ( 1, Point -1 -1 1 )
        , ( 2, Point -1 1 -1 )
        , ( 3, Point 1 -1 -1 )
        , ( 4, Point 1 1 1 )
        ]
            |> Dict.fromList
            |> Dict.map (\_ -> vectorScale (1 / sqrt 3))
    , faces =
        [ ( 1, [ 3, 2, 1 ] )
        , ( 2, [ 4, 2, 3 ] )
        , ( 3, [ 4, 3, 1 ] )
        , ( 4, [ 4, 1, 2 ] )
        ]
            |> Dict.fromList
    }


cube : Mesh
cube =
    { vertices =
        [ ( 1, Point -1 -1 -1 )
        , ( 2, Point -1 -1 1 )
        , ( 3, Point -1 1 -1 )
        , ( 4, Point -1 1 1 )
        , ( 5, Point 1 -1 -1 )
        , ( 6, Point 1 -1 1 )
        , ( 7, Point 1 1 -1 )
        , ( 8, Point 1 1 1 )
        ]
            |> Dict.fromList
            |> Dict.map (\_ -> vectorScale (1 / sqrt 3))
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


octahedron : Mesh
octahedron =
    { vertices =
        [ ( 1, Point -1 0 0 )
        , ( 2, Point 1 0 0 )
        , ( 3, Point 0 -1 0 )
        , ( 4, Point 0 1 0 )
        , ( 5, Point 0 0 -1 )
        , ( 6, Point 0 0 1 )
        ]
            |> Dict.fromList
            |> Dict.map (\_ -> vectorScale 1)
    , faces =
        [ ( 1, [ 4, 1, 5 ] )
        , ( 2, [ 4, 5, 2 ] )
        , ( 3, [ 4, 2, 6 ] )
        , ( 4, [ 4, 6, 1 ] )
        , ( 5, [ 3, 1, 6 ] )
        , ( 6, [ 3, 6, 2 ] )
        , ( 7, [ 3, 2, 5 ] )
        , ( 8, [ 3, 5, 1 ] )
        ]
            |> Dict.fromList
    }


phi : Float
phi =
    (1 + sqrt 5) / 2


icosahedron : Mesh
icosahedron =
    { vertices =
        [ ( 1, Point -phi -1 0 )
        , ( 2, Point -phi 1 0 )
        , ( 3, Point phi -1 0 )
        , ( 4, Point phi 1 0 )
        , ( 5, Point -1 0 -phi )
        , ( 6, Point -1 0 phi )
        , ( 7, Point 1 0 -phi )
        , ( 8, Point 1 0 phi )
        , ( 9, Point 0 -phi -1 )
        , ( 10, Point 0 -phi 1 )
        , ( 11, Point 0 phi -1 )
        , ( 12, Point 0 phi 1 )
        ]
            |> Dict.fromList
            |> Dict.map (\_ -> vectorScale (1 / sqrt (phi ^ 2 + 1)))
    , faces =
        [ ( 1, [ 1, 2, 6 ] )
        , ( 2, [ 2, 1, 5 ] )
        , ( 3, [ 3, 4, 7 ] )
        , ( 4, [ 4, 3, 8 ] )
        , ( 5, [ 5, 7, 11 ] )
        , ( 6, [ 7, 5, 9 ] )
        , ( 7, [ 6, 8, 10 ] )
        , ( 8, [ 8, 6, 12 ] )
        , ( 9, [ 9, 10, 3 ] )
        , ( 10, [ 10, 9, 1 ] )
        , ( 11, [ 12, 11, 4 ] )
        , ( 12, [ 11, 12, 2 ] )
        , ( 13, [ 12, 4, 8 ] )
        , ( 14, [ 12, 6, 2 ] )
        , ( 15, [ 11, 7, 4 ] )
        , ( 16, [ 11, 2, 5 ] )
        , ( 17, [ 10, 8, 3 ] )
        , ( 18, [ 10, 1, 6 ] )
        , ( 19, [ 9, 3, 7 ] )
        , ( 20, [ 9, 5, 1 ] )
        ]
            |> Dict.fromList
    }


invPhi : Float
invPhi =
    1 / phi


dodecahedron : Mesh
dodecahedron =
    { vertices =
        [ ( 1, Point -1 -1 -1 )
        , ( 2, Point -1 -1 1 )
        , ( 3, Point -1 1 -1 )
        , ( 4, Point -1 1 1 )
        , ( 5, Point 1 -1 -1 )
        , ( 6, Point 1 -1 1 )
        , ( 7, Point 1 1 -1 )
        , ( 8, Point 1 1 1 )
        , ( 9, Point -invPhi -phi 0 )
        , ( 10, Point -invPhi phi 0 )
        , ( 11, Point invPhi -phi 0 )
        , ( 12, Point invPhi phi 0 )
        , ( 13, Point -phi 0 -invPhi )
        , ( 14, Point -phi 0 invPhi )
        , ( 15, Point phi 0 -invPhi )
        , ( 16, Point phi 0 invPhi )
        , ( 17, Point 0 -invPhi -phi )
        , ( 18, Point 0 -invPhi phi )
        , ( 19, Point 0 invPhi -phi )
        , ( 20, Point 0 invPhi phi )
        ]
            |> Dict.fromList
            |> Dict.map (\_ -> vectorScale (1 / sqrt (invPhi ^ 2 + phi ^ 2)))
    , faces =
        [ ( 1, [ 8, 16, 6, 18, 20 ] )
        , ( 2, [ 8, 20, 4, 10, 12 ] )
        , ( 3, [ 16, 8, 12, 7, 15 ] )
        , ( 4, [ 6, 16, 15, 5, 11 ] )
        , ( 5, [ 18, 6, 11, 9, 2 ] )
        , ( 6, [ 20, 18, 2, 14, 4 ] )
        , ( 7, [ 1, 17, 19, 3, 13 ] )
        , ( 8, [ 1, 13, 14, 2, 9 ] )
        , ( 9, [ 17, 1, 9, 11, 5 ] )
        , ( 10, [ 19, 17, 5, 15, 7 ] )
        , ( 11, [ 3, 19, 7, 12, 10 ] )
        , ( 12, [ 13, 3, 10, 4, 14 ] )
        ]
            |> Dict.fromList
    }
