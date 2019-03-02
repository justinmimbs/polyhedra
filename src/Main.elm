port module Main exposing (main)

import Browser
import Brush exposing (Brush, Point2D)
import Dict exposing (Dict)
import Geometry exposing (..)
import Html exposing (Html)
import Html.Attributes
import Mesh exposing (Mesh, SuperMesh, reify)
import Polyhedron exposing (bitruncate, cube, dodecahedron, icosahedron, octahedron, tetrahedron, truncate)
import Render
import Slider exposing (Slider)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( init Icosahedron, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }


init : Polyhedron -> Model
init polyhedron =
    { selected = polyhedron
    , orientation =
        quaternionMultiply
            (quaternionFromAxisAngle (Vector 0 1 0) (pi / 5.5))
            (quaternionFromAxisAngle (Vector 1 0 0) (-pi / 6.5))
    , viewportOrientation = TaitBryan 0 0
    , slider = Slider.init layout.sliderLength 0
    , brushing = Nothing
    , mode = Transform
    }


type alias Model =
    { selected : Polyhedron
    , orientation : Quaternion
    , viewportOrientation : TaitBryan
    , slider : Slider
    , brushing : Maybe ( BrushTarget, Brush )
    , mode : Mode
    }


type Polyhedron
    = Tetrahedron
    | Cube
    | Octahedron
    | Icosahedron
    | Dodecahedron


type BrushTarget
    = SliderPosition
    | ObjectRotation


type Mode
    = Transform
    | Select


{-| We're not concerned with yaw.
-}
type alias TaitBryan =
    { pitch : Float
    , roll : Float
    }



-- polyhedron


polyhedronList : List Polyhedron
polyhedronList =
    [ Tetrahedron, Cube, Octahedron, Icosahedron, Dodecahedron ]


polyhedronData : Polyhedron -> { name : String, mesh : Mesh, truncation : SuperMesh, bitruncation : SuperMesh }
polyhedronData =
    let
        toData name mesh =
            { name = name
            , mesh = mesh
            , truncation = truncate mesh
            , bitruncation = bitruncate mesh
            }

        data =
            { tetrahedron = toData "tetrahedron" tetrahedron
            , cube = toData "cube" cube
            , octahedron = toData "octahedron" octahedron
            , icosahedron = toData "icosahedron" icosahedron
            , dodecahedron = toData "dodecahedron" dodecahedron
            }
    in
    \polyhedron ->
        case polyhedron of
            Tetrahedron ->
                data.tetrahedron

            Cube ->
                data.cube

            Octahedron ->
                data.octahedron

            Icosahedron ->
                data.icosahedron

            Dodecahedron ->
                data.dodecahedron



-- update


type Msg
    = BrushStarted BrushTarget Point2D
    | BrushMoved Point2D
    | BrushEnded
    | ModeSelected Mode
    | PolyhedronSelected Polyhedron
    | ViewportRotated TaitBryan


update : Msg -> Model -> Model
update msg ({ orientation, slider, brushing } as model) =
    case msg of
        BrushStarted brushTarget point ->
            { model | brushing = Just ( brushTarget, Brush.init point ) }

        BrushMoved point ->
            { model | brushing = brushing |> Maybe.map (Tuple.mapSecond (Brush.update point)) }

        BrushEnded ->
            case brushing of
                Just ( SliderPosition, brush ) ->
                    { model
                        | slider = slider |> Slider.applyBrush brush
                        , brushing = Nothing
                    }

                Just ( ObjectRotation, brush ) ->
                    { model
                        | orientation = quaternionMultiply orientation (rotationFromBrush brush)
                        , brushing = Nothing
                    }

                Nothing ->
                    model

        ModeSelected mode ->
            { model | mode = mode }

        PolyhedronSelected polyhedron ->
            { model | selected = polyhedron }

        ViewportRotated viewportOrientation ->
            { model | viewportOrientation = viewportOrientation }


rotationFromBrush : Brush -> Quaternion
rotationFromBrush { from, to } =
    let
        dx =
            to.x - from.x

        dy =
            to.y - from.y

        axis =
            Vector -dy -dx 0 |> vectorNormalize

        angle =
            -- brushing a distance of the figure diameter gives a half-circle rotation
            (sqrt (dx * dx + dy * dy) / (layout.figureRadius * 2)) * pi
    in
    quaternionFromAxisAngle axis angle



-- ports


port onViewportRotation : (TaitBryan -> msg) -> Sub msg



-- events


subscriptions : Model -> Sub Msg
subscriptions =
    let
        brushSubscriptions =
            Sub.batch
                [ Brush.subscriptions BrushMoved BrushEnded
                , onViewportRotation ViewportRotated
                ]
    in
    \{ brushing } ->
        case brushing of
            Just _ ->
                brushSubscriptions

            Nothing ->
                onViewportRotation ViewportRotated



-- view


layout =
    { sliderLength = 240.0
    , figureRadius = 160.0
    }


lightDirection : Vector
lightDirection =
    Vector -2 -3 -2 |> vectorNormalize


faceClass : Dict Int a -> Int -> String
faceClass faces f =
    if Dict.member f faces then
        "face a"

    else
        "face b"


view : Model -> Browser.Document Msg
view { selected, orientation, viewportOrientation, slider, brushing, mode } =
    let
        spacing =
            60.0

        rotationMatrix =
            quaternionMultiply
                (case brushing of
                    Just ( ObjectRotation, brush ) ->
                        quaternionMultiply orientation (rotationFromBrush brush)

                    _ ->
                        orientation
                )
                (quaternionFromTaitBryan 0 (-0.5 * viewportOrientation.roll) (-0.5 * viewportOrientation.pitch))
                |> quaternionToMatrix

        sliderBrushing =
            case brushing of
                Just ( SliderPosition, brush ) ->
                    Just brush

                _ ->
                    Nothing

        t =
            Slider.value sliderBrushing slider * 2
    in
    Browser.Document
        "Polyhedra"
        [ Svg.svg
            (case brushing of
                Just _ ->
                    [ Brush.touchMove BrushMoved
                    , Brush.touchEnd BrushEnded
                    ]

                Nothing ->
                    []
            )
            [ Svg.svg
                [ Svg.Attributes.x "50%"
                , Svg.Attributes.y "50%"
                ]
                [ viewFigure 0 -spacing rotationMatrix t selected
                , Svg.g
                    []
                    (case mode of
                        Transform ->
                            [ viewSlider 0 layout.figureRadius sliderBrushing slider
                            , if t == 0 then
                                viewButton 0 (layout.figureRadius + spacing) iconEllipsis (ModeSelected Select)

                              else
                                Svg.text ""
                            ]

                        Select ->
                            [ viewText 0 -spacing (polyhedronData selected).name
                            , viewMenu 0 layout.figureRadius rotationMatrix selected
                            , viewButton 0 (layout.figureRadius + spacing) iconX (ModeSelected Transform)
                            ]
                    )
                ]
            ]
        ]


viewText : Float -> Float -> String -> Svg a
viewText cx cy string =
    Svg.text_
        [ translate cx cy
        ]
        [ Svg.text string
        ]


viewSlider : Float -> Float -> Maybe Brush -> Slider -> Svg Msg
viewSlider cx cy sliderBrushing slider =
    Svg.g
        [ translate (cx + layout.sliderLength / -2) cy
        ]
        [ Slider.view (BrushStarted SliderPosition) sliderBrushing slider
        ]


viewButton : Float -> Float -> Svg Msg -> Msg -> Svg Msg
viewButton cx cy icon msg =
    Svg.g
        [ translate cx cy
        ]
        [ icon
        , viewRect
            [ Svg.Attributes.opacity "0"
            , Svg.Events.onClick msg
            ]
            (centeredSquare 40)
        ]


viewFigure : Float -> Float -> Matrix -> Float -> Polyhedron -> Svg Msg
viewFigure cx cy rotationMatrix t polyhedron =
    let
        data =
            polyhedron |> polyhedronData

        mesh =
            if t <= 1.0 then
                reify t data.truncation

            else
                reify (t - 1.0) data.bitruncation

        radius =
            mesh.vertices
                |> Dict.foldl (\_ p -> max (vectorLengthSquared p)) 0
                |> sqrt

        matrix =
            rotationMatrix |> matrixScale (layout.figureRadius / radius)

        meshTransformed =
            { mesh | vertices = mesh.vertices |> Dict.map (\_ -> matrixMultiplyVector matrix) }
    in
    Svg.g
        [ Svg.Attributes.class (polyhedronData polyhedron).name
        , Svg.Attributes.transform
            ("translate(" ++ String.fromFloat cx ++ ", " ++ String.fromFloat cy ++ ") scale(1, -1)")
        ]
        [ Render.meshFigure lightDirection (faceClass data.mesh.faces) meshTransformed
        , Svg.circle
            [ Svg.Attributes.cx "0"
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r <| String.fromFloat layout.figureRadius
            , Svg.Attributes.opacity "0"
            , Brush.onStart (BrushStarted ObjectRotation)
            , Brush.touchStart (BrushStarted ObjectRotation)
            ]
            []
        ]


viewMenu : Float -> Float -> Matrix -> Polyhedron -> Svg Msg
viewMenu cx cy rotationMatrix selected =
    let
        iconSize =
            44.0

        iconSpacing =
            60.0

        offsetX =
            toFloat (List.length polyhedronList - 1) * iconSpacing / -2

        matrix =
            rotationMatrix |> matrixScale (iconSize / 2)
    in
    Svg.g
        [ translate cx cy
        ]
        (List.indexedMap
            (\i polyhedron ->
                let
                    { mesh } =
                        polyhedron |> polyhedronData

                    x =
                        offsetX + (toFloat i * iconSpacing)
                in
                Svg.g
                    [ Svg.Attributes.class (polyhedron == selected |> bool "selected" "")
                    , Svg.Attributes.transform
                        ("translate(" ++ String.fromFloat x ++ ", 0) scale(1, -1)")
                    ]
                    [ Render.meshIcon
                        { mesh | vertices = mesh.vertices |> Dict.map (\_ -> matrixMultiplyVector matrix) }
                    , viewRect
                        [ Svg.Attributes.opacity "0"
                        , Svg.Events.onClick (PolyhedronSelected polyhedron)
                        ]
                        (centeredSquare iconSize)
                    ]
            )
            polyhedronList
        )


centeredSquare : Float -> Rect
centeredSquare length =
    Rect (length / -2) (length / -2) length length



-- icons


iconEllipsis : Svg a
iconEllipsis =
    Svg.g
        [ Svg.Attributes.class "icon"
        , Svg.Attributes.transform "translate(0.5, 0.5)"
        ]
        ([ -1, 0, 1 ]
            |> List.map
                (\i ->
                    Svg.circle
                        [ Svg.Attributes.cx <| String.fromInt (i * 7)
                        , Svg.Attributes.cy "0"
                        , Svg.Attributes.r "1.5"
                        ]
                        []
                )
        )


iconX : Svg a
iconX =
    Svg.g
        [ Svg.Attributes.class "icon"
        , Svg.Attributes.transform "translate(0.5, 0.5)"
        ]
        [ viewLine ( -7.5, 7.5 ) ( 7.5, -7.5 )
        , viewLine ( 7.5, 7.5 ) ( -7.5, -7.5 )
        ]



-- svg


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


viewRect : List (Svg.Attribute a) -> Rect -> Svg a
viewRect attributes { x, y, width, height } =
    Svg.rect
        (Svg.Attributes.x (String.fromFloat x)
            :: Svg.Attributes.y (String.fromFloat y)
            :: Svg.Attributes.width (String.fromFloat width)
            :: Svg.Attributes.height (String.fromFloat height)
            :: attributes
        )
        []


viewLine : ( Float, Float ) -> ( Float, Float ) -> Svg a
viewLine ( x1, y1 ) ( x2, y2 ) =
    Svg.line
        [ Svg.Attributes.x1 <| String.fromFloat x1
        , Svg.Attributes.y1 <| String.fromFloat y1
        , Svg.Attributes.x2 <| String.fromFloat x2
        , Svg.Attributes.y2 <| String.fromFloat y2
        ]
        []


translate : Float -> Float -> Svg.Attribute a
translate x y =
    Svg.Attributes.transform
        ("translate(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")")



--


bool : a -> a -> Bool -> a
bool t f x =
    if x then
        t

    else
        f
