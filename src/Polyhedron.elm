module Polyhedron exposing
    ( truncate, bitruncate
    , cube, tetrahedron
    )

{-|

@docs truncate, bitruncate
@docs cube, tetrahedron

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
            |> Dict.map (\_ v -> vectorSubtract v centroid |> vectorScale 100)
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
            |> Dict.map (\_ v -> vectorSubtract v centroid |> vectorScale 100)
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
