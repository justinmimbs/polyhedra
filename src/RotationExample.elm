module RotationExample exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Geometry exposing (..)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Mesh exposing (Mesh, SuperMesh, faceNormal, faceToPolygon, reify)
import Polyhedron exposing (cube)
import Set exposing (Set)
import Slider exposing (Slider)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( Model quaternionIdentity Nothing, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { orientation : Quaternion
    , brushing : Maybe { from : Point2D, to : Point2D }
    }



-- update


type Msg
    = BrushStarted Point2D
    | BrushMoved Point2D
    | BrushEnded


type alias Point2D =
    { x : Float
    , y : Float
    }


update : Msg -> Model -> Model
update msg ({ brushing, orientation } as model) =
    case msg of
        BrushStarted from ->
            { model | brushing = Just { from = from, to = from } }

        BrushMoved to ->
            case brushing of
                Just { from } ->
                    { model | brushing = Just { from = from, to = to } }

                Nothing ->
                    model

        BrushEnded ->
            { model
                | brushing = Nothing
                , orientation =
                    brushing
                        |> Maybe.map (brushingToRotation >> quaternionMultiply orientation)
                        |> Maybe.withDefault orientation
            }


brushingToRotation : { from : Point2D, to : Point2D } -> Quaternion
brushingToRotation { from, to } =
    let
        dx =
            to.x - from.x

        dy =
            to.y - from.y

        axis =
            Vector -dy -dx 0 |> vectorNormalize

        angle =
            (sqrt (dx * dx + dy * dy) / 320) * pi
    in
    quaternionFromAxisAngle axis angle



-- events


subscriptions : Model -> Sub Msg
subscriptions { brushing } =
    case brushing of
        Just _ ->
            subBrushing

        Nothing ->
            Sub.none


subBrushing : Sub Msg
subBrushing =
    Sub.batch
        [ Browser.Events.onMouseMove (Decode.map BrushMoved decodeMousePosition)
        , Browser.Events.onMouseUp (Decode.succeed BrushEnded)
        , Browser.Events.onVisibilityChange (always BrushEnded)
        ]


decodeMousePosition : Decoder Point2D
decodeMousePosition =
    Decode.map2 Point2D
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)



-- view


view : Model -> Browser.Document Msg
view =
    let
        cameraMatrix : Matrix
        cameraMatrix =
            matrixLookAt (Vector 3 3 5) vectorZero

        lightDirection : Vector
        lightDirection =
            Vector -3 -6 1 |> matrixMultiplyVector cameraMatrix |> vectorNormalize
    in
    \{ orientation, brushing } ->
        let
            rotationMatrix =
                brushing
                    |> Maybe.map (brushingToRotation >> quaternionMultiply orientation)
                    |> Maybe.withDefault orientation
                    |> quaternionToMatrix

            matrix =
                matrixMultiply cameraMatrix rotationMatrix

            meshTransformed =
                { cube | vertices = cube.vertices |> Dict.map (\_ -> matrixMultiplyVector matrix) }
        in
        Browser.Document
            "Polyhedra"
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
                    [ viewMesh lightDirection (always "face a") meshTransformed
                    , Svg.circle
                        [ Svg.Attributes.cx "0"
                        , Svg.Attributes.cy "0"
                        , Svg.Attributes.r "160"
                        , Svg.Attributes.opacity "0"
                        , Svg.Events.preventDefaultOn
                            "mousedown"
                            (decodeMousePosition |> Decode.map (\point -> ( BrushStarted point, True )))
                        ]
                        []
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
