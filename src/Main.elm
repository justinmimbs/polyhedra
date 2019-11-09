port module Main exposing (main)

import Browser
import Browser.Events
import Brush exposing (Brush, Point2D)
import Dict exposing (Dict)
import Geometry exposing (..)
import Html exposing (Html)
import Html.Attributes
import Mesh exposing (Mesh, SuperMesh)
import Polyhedron exposing (cube, dodecahedron, icosahedron, octahedron, tetrahedron)
import Render
import Slider exposing (Slider)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( init Icosahedron, Cmd.none )
        , update = \msg model -> ( update msg model, command msg )
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
    , viewportOrientation = Nothing
    , slider = Slider.init layout.sliderLength 0
    , brushing = Nothing
    , brushedRotation = Nothing
    , mode = Transform
    , needPermission = False
    }


type alias Model =
    { selected : Polyhedron
    , orientation : Quaternion
    , viewportOrientation : Maybe ViewportOrientation
    , slider : Slider
    , brushing : Maybe ( BrushTarget, Brush )
    , brushedRotation : Maybe MovingPoint
    , mode : Mode
    , needPermission : Bool -- Device Motion/Orientation Permission
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


type alias MovingPoint =
    { point : Point2D
    , velocity : Point2D
    }


type Mode
    = Transform
    | Select


type alias ViewportOrientation =
    { initial : TaitBryan
    , current : TaitBryan
    }


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
            , truncation = Polyhedron.truncate mesh
            , bitruncation = Polyhedron.bitruncate mesh
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


dual : Polyhedron -> Polyhedron
dual polyhedron =
    case polyhedron of
        Tetrahedron ->
            Tetrahedron

        Cube ->
            Octahedron

        Octahedron ->
            Cube

        Icosahedron ->
            Dodecahedron

        Dodecahedron ->
            Icosahedron



-- update


type Msg
    = BrushStarted BrushTarget Brush
    | BrushMoved Brush
    | BrushEnded
    | SelectedMode Mode
    | SelectedPolyhedron Polyhedron
    | ReceivedNeedPermission Bool
    | ClickedRequestPermission
    | ViewportRotated TaitBryan
    | AnimationStepped Float


update : Msg -> Model -> Model
update msg ({ orientation, slider, brushing, brushedRotation } as model) =
    case msg of
        BrushStarted SliderPosition brush ->
            { model | brushing = Just ( SliderPosition, brush ) }

        BrushStarted ObjectRotation brush ->
            { model
                | brushing = Just ( ObjectRotation, brush )
                , brushedRotation = Just { point = { x = 0, y = 0 }, velocity = { x = 0, y = 0 } }
                , orientation =
                    case brushedRotation of
                        Just brushed ->
                            -- apply previous brushedRotation
                            quaternionMultiply orientation (rotationFromBrushDelta brushed.point)

                        Nothing ->
                            orientation
            }

        BrushMoved brush ->
            { model | brushing = brushing |> Maybe.map (Tuple.mapSecond (always brush)) }

        BrushEnded ->
            case brushing of
                Just ( SliderPosition, brush ) ->
                    { model
                        | slider = slider |> Slider.applyBrush brush
                        , brushing = Nothing
                    }

                Just ( ObjectRotation, brush ) ->
                    { model
                        | brushing = Nothing
                    }

                Nothing ->
                    model

        SelectedMode mode ->
            { model | mode = mode }

        SelectedPolyhedron polyhedron ->
            if Slider.value Nothing slider == 1 then
                { model | selected = dual polyhedron }

            else
                { model | selected = polyhedron }

        ReceivedNeedPermission needPermission ->
            { model | needPermission = needPermission }

        ClickedRequestPermission ->
            model

        ViewportRotated angle ->
            { model
                | viewportOrientation =
                    case model.viewportOrientation of
                        Just { initial } ->
                            Just { initial = initial, current = angle }

                        Nothing ->
                            Just { initial = angle, current = angle }
            }

        AnimationStepped ms ->
            if 0 < ms then
                case brushedRotation of
                    Just brushed ->
                        let
                            nextBrushedRotation =
                                brushed |> updateBrushedRotation brushing (ms / 1000)
                        in
                        { model
                            | brushedRotation = nextBrushedRotation
                            , orientation =
                                case nextBrushedRotation of
                                    Nothing ->
                                        -- rotation has stablized; apply brushedRotation
                                        quaternionMultiply orientation (rotationFromBrushDelta brushed.point)

                                    Just _ ->
                                        orientation
                        }

                    Nothing ->
                        model

            else
                model


updateBrushedRotation : Maybe ( BrushTarget, Brush ) -> Float -> MovingPoint -> Maybe MovingPoint
updateBrushedRotation brushing dt previous =
    let
        nextPoint =
            case brushing of
                Just ( ObjectRotation, { from, to } ) ->
                    updateMovingPoint dt previous
                        |> interpolatePoints 0.6
                            { x = to.x - from.x
                            , y = to.y - from.y
                            }

                _ ->
                    updateMovingPoint dt previous

        next =
            { point =
                nextPoint
            , velocity =
                { x = (nextPoint.x - previous.point.x) / dt
                , y = (nextPoint.y - previous.point.y) / dt
                }
            }
    in
    if brushing == Nothing && abs next.velocity.x < 0.001 && abs next.velocity.y < 0.001 then
        -- stablized
        Nothing

    else
        Just next


updateMovingPoint : Float -> MovingPoint -> Point2D
updateMovingPoint dt { point, velocity } =
    { x = point.x + velocity.x * dt * 0.8
    , y = point.y + velocity.y * dt * 0.8
    }


interpolatePoints : Float -> Point2D -> Point2D -> Point2D
interpolatePoints t a b =
    { x = a.x + (b.x - a.x) * t
    , y = a.y + (b.y - a.y) * t
    }


rotationFromBrushDelta : Point2D -> Quaternion
rotationFromBrushDelta { x, y } =
    let
        axis =
            Vector -y -x 0 |> vectorNormalize

        angle =
            -- brushing a distance of the figure diameter gives a half-circle rotation
            (sqrt (x * x + y * y) / (layout.figureRadius * 2)) * pi
    in
    quaternionFromAxisAngle axis angle


{-| Convert from a device to a screen coordinate system (by swapping and
negating pitch and roll).

    - device axes = x -> forward, y -> right, z -> down (i.e. roll, pitch, yaw)
    - screen axes = x -> right, y -> up, z -> forward (i.e. left-handed)

Also decrease the effect of the device orientation by some factor (0.5).

-}
rotationFromViewportOrientation : ViewportOrientation -> Quaternion
rotationFromViewportOrientation { initial, current } =
    let
        deltaPitch =
            current.pitch - initial.pitch

        deltaRoll =
            current.roll - initial.roll
    in
    quaternionFromTaitBryan 0 (0.5 * -deltaRoll) (0.5 * -deltaPitch)



-- commands


command : Msg -> Cmd Msg
command msg =
    case msg of
        ClickedRequestPermission ->
            requestPermission ()

        _ ->
            Cmd.none



-- ports


port onViewportRotation : (TaitBryan -> msg) -> Sub msg


port onNeedPermission : (Bool -> msg) -> Sub msg


port requestPermission : () -> Cmd msg



-- events


subscriptions : Model -> Sub Msg
subscriptions { brushing, brushedRotation } =
    Sub.batch
        [ onViewportRotation ViewportRotated
        , onNeedPermission ReceivedNeedPermission
        , if brushing /= Nothing then
            Browser.Events.onVisibilityChange (always BrushEnded)

          else
            Sub.none
        , if brushedRotation /= Nothing then
            Browser.Events.onAnimationFrameDelta AnimationStepped

          else
            Sub.none
        ]



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
view { selected, orientation, viewportOrientation, slider, brushing, brushedRotation, mode, needPermission } =
    let
        spacing =
            60.0

        rotationMatrix =
            -- orientation * brushing * viewportOrientation
            quaternionMultiply
                (case brushedRotation of
                    Just brushed ->
                        quaternionMultiply orientation (rotationFromBrushDelta brushed.point)

                    Nothing ->
                        orientation
                )
                (case viewportOrientation of
                    Just vo ->
                        rotationFromViewportOrientation vo

                    Nothing ->
                        quaternionIdentity
                )
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
                Just ( _, brush ) ->
                    Brush.onBrush BrushMoved BrushEnded brush

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
                            , if t == 0 || t == 2 then
                                viewButton 0 (layout.figureRadius + spacing) iconEllipsis (SelectedMode Select)

                              else
                                Svg.text ""
                            ]

                        Select ->
                            let
                                displayed : Polyhedron
                                displayed =
                                    t == 2 |> bool (dual selected) selected
                            in
                            [ viewText 0 -spacing (polyhedronData displayed).name
                            , viewMenu 0 layout.figureRadius rotationMatrix displayed
                            , viewButton 0 (layout.figureRadius + spacing) iconX (SelectedMode Transform)
                            ]
                    )
                , if needPermission then
                    viewButton (layout.sliderLength / 2) (layout.figureRadius + spacing) iconRotation ClickedRequestPermission

                  else
                    Svg.text ""
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
                Mesh.reify t data.truncation

            else
                Mesh.reify (t - 1.0) data.bitruncation

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
            (Brush.onStart (BrushStarted ObjectRotation)
                ++ [ Svg.Attributes.cx "0"
                   , Svg.Attributes.cy "0"
                   , Svg.Attributes.r <| String.fromFloat layout.figureRadius
                   , Svg.Attributes.opacity "0"
                   ]
            )
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
                        , Svg.Events.onClick (SelectedPolyhedron polyhedron)
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


iconRotation : Svg a
iconRotation =
    Svg.g
        [ Svg.Attributes.class "icon"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M -8.5 -2.5 L -11 0 L -8 3 M -2.5 8.5 L 0 11 L 3 8 M 8 3 L 11 0 L 8.5 -2.5 M 3 -8 L 0 -11 L -2.5 -8.5 M -10 0 C -8 1 -4 2 0 2 C 4 2 8 1 10 0 M 0 -10 C 1 -8 2 -4 2 0 C 2 4 1 8 0 10"
            ]
            []
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
