module Geometry exposing
    ( Vector, vectorZero, vectorLength, vectorLengthSquared, vectorNormalize, vectorScale, vectorAdd, vectorSubtract, vectorDot, vectorCross
    , Matrix, matrixIdentity, matrixScale, matrixLookAt, matrixMultiply
    , Point, direction, distanceSquared, interpolate, midpoint
    , Polygon, polygonCenter, polygonIsClockwise
    )

{-|

@docs Vector, vectorZero, vectorLength, vectorLengthSquared, vectorNormalize, vectorScale, vectorAdd, vectorSubtract, vectorDot, vectorCross
@docs Matrix, matrixIdentity, matrixScale, matrixLookAt, matrixMultiply
@docs Point, direction, distanceSquared, interpolate, midpoint
@docs Polygon, polygonCenter, polygonIsClockwise

-}

import Extra.List as List


type alias Vector =
    { x : Float
    , y : Float
    , z : Float
    }


vectorZero : Vector
vectorZero =
    Vector 0 0 0


vectorLength : Vector -> Float
vectorLength { x, y, z } =
    sqrt (x * x + y * y + z * z)


vectorLengthSquared : Vector -> Float
vectorLengthSquared { x, y, z } =
    x * x + y * y + z * z


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


vectorScale : Float -> Vector -> Vector
vectorScale factor { x, y, z } =
    { x = x * factor
    , y = y * factor
    , z = z * factor
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


vectorDot : Vector -> Vector -> Float
vectorDot a b =
    (a.x * b.x)
        + (a.y * b.y)
        + (a.z * b.z)


vectorCross : Vector -> Vector -> Vector
vectorCross a b =
    { x = a.y * b.z - a.z * b.y
    , y = a.z * b.x - a.x * b.z
    , z = a.x * b.y - a.y * b.x
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


matrixScale : Float -> Matrix -> Matrix
matrixScale factor { a, b, c, t } =
    { a = a |> vectorScale factor
    , b = b |> vectorScale factor
    , c = c |> vectorScale factor
    , t = t |> vectorScale factor
    }


matrixMultiply : Matrix -> Vector -> Vector
matrixMultiply { a, b, c, t } { x, y, z } =
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


direction : Point -> Point -> Vector
direction a b =
    vectorSubtract b a |> vectorNormalize


distanceSquared : Point -> Point -> Float
distanceSquared a b =
    (a.x - b.x) ^ 2 + (a.y - b.y) ^ 2 + (a.z - b.z) ^ 2



--


type alias Polygon =
    List Point


{-| Assume regular polygon and take arithmetic mean of points.
-}
polygonCenter : Polygon -> Point
polygonCenter points =
    case List.length points of
        0 ->
            vectorZero

        n ->
            points |> List.foldl vectorAdd vectorZero |> vectorScale (1 / toFloat n)


{-| Based on the shoelace formula for area of a simple polygon (where `area =
abs (area2 / 2)`).

The value of area2 will be positive when points are ordered clockwise, zero
when collinear, and negative when counterclockwise.

-}
polygonIsClockwise : Polygon -> Bool
polygonIsClockwise points =
    let
        area2 =
            List.foldCycle
                (\a b result -> (b.x - a.x) * (b.y + a.y) + result)
                0
                points
    in
    0 < area2
