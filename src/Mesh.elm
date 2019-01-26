module Mesh exposing
    ( Mesh
    , Face, faceToPolygon, faceNormal
    , SuperMesh, SuperFace, SuperVertex, reify
    )

{-|

@docs Mesh
@docs Face, faceToPolygon, faceNormal
@docs SuperMesh, SuperFace, SuperVertex, reify

-}

import Dict exposing (Dict)
import Extra.Dict as Dict
import Extra.List as List
import Geometry exposing (..)
import Set exposing (Set)


type alias Mesh =
    { vertices : Dict Int Point
    , faces : Dict Int Face
    }


mergeCoincidentVertices : Mesh -> Mesh
mergeCoincidentVertices ({ vertices, faces } as mesh) =
    let
        -- { points : List ( Point, Int ) -- assoc list; maps each unique point to a canonical vertex
        -- , vertexMap : Dict Int Int -- maps every vertex to its canonical vertex
        -- }
        { points, vertexMap } =
            vertices
                |> Dict.foldl
                    (\v p r ->
                        case List.find (Tuple.first >> pointEqual p) r.points of
                            Nothing ->
                                { points = ( p, v ) :: r.points
                                , vertexMap = r.vertexMap |> Dict.insert v v
                                }

                            Just ( _, u ) ->
                                { points = r.points
                                , vertexMap = r.vertexMap |> Dict.insert v u
                                }
                    )
                    { points = [], vertexMap = Dict.empty }
    in
    if Dict.size vertices == List.length points then
        mesh

    else
        let
            usedVertices : Set Int
            usedVertices =
                points |> List.map Tuple.second |> Set.fromList
        in
        { vertices =
            vertices |> Dict.filter (\v _ -> Set.member v usedVertices)
        , faces =
            faces
                |> Dict.map
                    (\_ face ->
                        face
                            |> List.map (Dict.lookup 0 vertexMap)
                            -- remove consecutive duplicates
                            |> List.foldCycle
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


pointEqual : Point -> Point -> Bool
pointEqual a b =
    distanceSquared a b <= 0.000000001



--


type alias Face =
    List Int


faceToPolygon : Dict Int Point -> Face -> Polygon
faceToPolygon vertices face =
    List.map
        (Dict.lookup vectorZero vertices)
        face


faceNormal : Polygon -> Vector
faceNormal points =
    case points of
        a :: b :: c :: _ ->
            vectorCross
                (vectorSubtract b a)
                (vectorSubtract b c)
                |> vectorNormalize

        _ ->
            Vector 0 1 0



-- super


{-| Represents a continuum between two shapes, as described by one set of faces
and two sets of vertex positions, where a uniform interpolation between all
corresponding vertex points yields a valid shape.
-}
type alias SuperMesh =
    { vertices0 : Dict SuperVertex Point
    , vertices1 : Dict SuperVertex Point
    , faces : Dict Int SuperFace
    }


type alias SuperFace =
    List SuperVertex


type alias SuperVertex =
    ( Int, Int )


indexVertices : Dict SuperVertex Point -> Dict Int SuperFace -> Mesh
indexVertices vertices faces =
    let
        vuToIndex =
            vertices |> Dict.keys |> List.indexedMap (\i vu -> ( vu, i )) |> Dict.fromList
    in
    { vertices =
        vertices |> Dict.foldl (\vu -> Dict.insert (Dict.lookup 0 vuToIndex vu)) Dict.empty
    , faces =
        faces |> Dict.map (\_ -> List.map (Dict.lookup 0 vuToIndex))
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
