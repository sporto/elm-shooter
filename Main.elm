module Main exposing (..)

import Debug
import Html exposing (..)


--import Svg
--import Svg.Attributes as SA

import Keyboard
import AnimationFrame
import Html.Attributes exposing (width, height, style)
import WebGL exposing (Mesh, Shader)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)


-- MODELS


type alias Model =
    { x : Int
    , y : Int
    }


type Key
    = Up
    | Down
    | Left
    | Right
    | OtherKey



-- MSGs


type Msg
    = NoOp
    | OnKeyboard Keyboard.KeyCode



-- INIT


initialModel : Model
initialModel =
    { x = 0
    , y = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs OnKeyboard


view : Model -> Html msg
view model =
    WebGL.toHtml
        [ width 400
        , height 200
        , style [ ( "display", "block" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            { perspective = perspective (1) }
        ]


perspective : Float -> Mat4
perspective t =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 1 1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
        ]



-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]


keyCodeToKey : Keyboard.KeyCode -> Key
keyCodeToKey keyCode =
    case keyCode of
        38 ->
            Up

        40 ->
            Down

        37 ->
            Left

        39 ->
            Right

        _ ->
            OtherKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        OnKeyboard keyCode ->
            case keyCodeToKey keyCode of
                Up ->
                    ( { model | y = model.y - 1 }, Cmd.none )

                Down ->
                    ( { model | y = model.y + 1 }, Cmd.none )

                Left ->
                    ( { model | x = model.x - 1 }, Cmd.none )

                Right ->
                    ( { model | x = model.x + 1 }, Cmd.none )

                OtherKey ->
                    ( model, Cmd.none )


main =
    Html.program
        { update = update
        , view = view
        , subscriptions = subscriptions
        , init = init
        }
