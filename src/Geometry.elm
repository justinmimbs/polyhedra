module Geometry exposing
    ( Vector, vectorZero, vectorLength, vectorLengthSquared, vectorNormalize, vectorScale, vectorAdd, vectorSubtract, vectorDot, vectorCross
    , Matrix, matrixIdentity, matrixScale, matrixLookAt, matrixMultiply, matrixMultiplyVector
    , Point, direction, distanceSquared, interpolate, midpoint
    , Polygon, polygonCenter, polygonIsClockwise
    , Quaternion, quaternionIdentity, quaternionFromAxisAngle, quaternionMultiply, quaternionToMatrix
    )

{-|

@docs Vector, vectorZero, vectorLength, vectorLengthSquared, vectorNormalize, vectorScale, vectorAdd, vectorSubtract, vectorDot, vectorCross
@docs Matrix, matrixIdentity, matrixScale, matrixLookAt, matrixMultiply, matrixMultiplyVector
@docs Point, direction, distanceSquared, interpolate, midpoint
@docs Polygon, polygonCenter, polygonIsClockwise
@docs Quaternion, quaternionIdentity, quaternionFromAxisAngle, quaternionMultiply, quaternionToMatrix

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


matrixMultiply : Matrix -> Matrix -> Matrix
matrixMultiply m1 m2 =
    { a =
        Vector
            (m1.a.x * m2.a.x + m1.a.y * m2.b.x + m1.a.z * m2.c.x)
            (m1.a.x * m2.a.y + m1.a.y * m2.b.y + m1.a.z * m2.c.y)
            (m1.a.x * m2.a.z + m1.a.y * m2.b.z + m1.a.z * m2.c.z)
    , b =
        Vector
            (m1.b.x * m2.a.x + m1.b.y * m2.b.x + m1.b.z * m2.c.x)
            (m1.b.x * m2.a.y + m1.b.y * m2.b.y + m1.b.z * m2.c.y)
            (m1.b.x * m2.a.z + m1.b.y * m2.b.z + m1.b.z * m2.c.z)
    , c =
        Vector
            (m1.c.x * m2.a.x + m1.c.y * m2.b.x + m1.c.z * m2.c.x)
            (m1.c.x * m2.a.y + m1.c.y * m2.b.y + m1.c.z * m2.c.y)
            (m1.c.x * m2.a.z + m1.c.y * m2.b.z + m1.c.z * m2.c.z)
    , t =
        Vector
            (m1.t.x * m2.a.x + m1.t.y * m2.b.x + m1.t.z * m2.c.x)
            (m1.t.x * m2.a.y + m1.t.y * m2.b.y + m1.t.z * m2.c.y)
            (m1.t.x * m2.a.z + m1.t.y * m2.b.z + m1.t.z * m2.c.z)
    }


matrixMultiplyVector : Matrix -> Vector -> Vector
matrixMultiplyVector { a, b, c, t } { x, y, z } =
    { x = x * a.x + y * b.x + z * c.x + t.x
    , y = x * a.y + y * b.y + z * c.y + t.y
    , z = x * a.z + y * b.z + z * c.z + t.z
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



--


type alias Quaternion =
    ( Float, Vector )


quaternionIdentity : Quaternion
quaternionIdentity =
    ( 1, vectorZero )


quaternionFromAxisAngle : Vector -> Float -> Quaternion
quaternionFromAxisAngle axis angle =
    if angle /= 0 then
        ( cos (angle / 2)
        , vectorScale (sin (angle / 2)) axis
        )

    else
        quaternionIdentity


quaternionMultiply : Quaternion -> Quaternion -> Quaternion
quaternionMultiply ( s1, v1 ) ( s2, v2 ) =
    ( s1 * s2 - vectorDot v1 v2
    , vectorScale s1 v2
        |> vectorAdd (vectorScale s2 v1)
        |> vectorAdd (vectorCross v1 v2)
    )


quaternionToMatrix : Quaternion -> Matrix
quaternionToMatrix ( s, { x, y, z } ) =
    { a = Vector (1 - 2 * y * y - 2 * z * z) (2 * x * y - 2 * s * z) (2 * x * z + 2 * s * y)
    , b = Vector (2 * x * y + 2 * s * z) (1 - 2 * x * x - 2 * z * z) (2 * y * z - 2 * s * x)
    , c = Vector (2 * x * z - 2 * s * y) (2 * y * z + 2 * s * x) (1 - 2 * x * x - 2 * y * y)
    , t = vectorZero
    }
