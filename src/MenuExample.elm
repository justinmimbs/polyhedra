module MenuExample exposing (main)

import Browser
import Dict exposing (Dict)
import Geometry exposing (..)
import Html exposing (Html)
import Html.Attributes
import Mesh exposing (Mesh)
import Polyhedron exposing (cube, dodecahedron, icosahedron, octahedron, tetrahedron)
import Render
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


main : Program () Polyhedron Polyhedron
main =
    Browser.document
        { init = \_ -> ( Tetrahedron, Cmd.none )
        , update = \msg model -> ( msg, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Polyhedron
    = Tetrahedron
    | Cube
    | Octahedron
    | Icosahedron
    | Dodecahedron


view : Polyhedron -> Browser.Document Polyhedron
view selected =
    Browser.Document
        "MenuExample"
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
                [ viewMenu initialOrientation selected
                , Svg.g
                    [ Svg.Attributes.transform "translate(0, -60)"
                    ]
                    [ iconEllipsis
                    ]
                , Svg.g
                    [ Svg.Attributes.transform "translate(0, -120)"
                    ]
                    [ iconX
                    ]
                ]
            ]
        ]


initialOrientation : Quaternion
initialOrientation =
    quaternionMultiply
        (quaternionFromAxisAngle (Vector 0 1 0) (pi / 5.5))
        (quaternionFromAxisAngle (Vector 1 0 0) (-pi / 6.5))


polyhedronList : List Polyhedron
polyhedronList =
    [ Tetrahedron, Cube, Octahedron, Icosahedron, Dodecahedron ]


polyhedronMesh : Polyhedron -> Mesh
polyhedronMesh polyhedron =
    case polyhedron of
        Tetrahedron ->
            tetrahedron

        Cube ->
            cube

        Octahedron ->
            octahedron

        Icosahedron ->
            icosahedron

        Dodecahedron ->
            dodecahedron


viewMenu : Quaternion -> Polyhedron -> Svg Polyhedron
viewMenu orientation selected =
    let
        matrix =
            orientation |> quaternionToMatrix |> matrixScale 26
    in
    Svg.g
        []
        (List.indexedMap
            (\i polyhedron ->
                let
                    mesh =
                        polyhedronMesh polyhedron
                in
                Svg.g
                    [ Svg.Attributes.class (polyhedron == selected |> bool "selected" "")
                    , Svg.Attributes.transform <| "translate(" ++ String.fromInt (-120 + (i * 60)) ++ ", 26) "
                    ]
                    [ Render.meshIcon
                        { mesh | vertices = mesh.vertices |> Dict.map (\_ -> matrixMultiplyVector matrix) }
                    , Svg.rect
                        [ Svg.Attributes.x "-24"
                        , Svg.Attributes.y "-24"
                        , Svg.Attributes.width "48"
                        , Svg.Attributes.height "48"
                        , Svg.Attributes.opacity "0"
                        , Svg.Events.onClick polyhedron
                        ]
                        []
                    ]
            )
            polyhedronList
        )


bool : a -> a -> Bool -> a
bool t f x =
    if x then
        t

    else
        f



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


viewLine : ( Float, Float ) -> ( Float, Float ) -> Svg a
viewLine ( x1, y1 ) ( x2, y2 ) =
    Svg.line
        [ Svg.Attributes.x1 <| String.fromFloat x1
        , Svg.Attributes.y1 <| String.fromFloat y1
        , Svg.Attributes.x2 <| String.fromFloat x2
        , Svg.Attributes.y2 <| String.fromFloat y2
        ]
        []
