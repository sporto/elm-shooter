module Main exposing (..)

import Debug
import Html exposing (..)
import Keyboard
import AnimationFrame
import Html.Attributes exposing (width, height, style)
import Collage exposing (..)
import Element
import Color


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
    collage 400
        400
        [ player
        ]
        |> Element.toHtml


player : Form
player =
    rect 20 20
        |> filled (Color.rgb 0 0 0)


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
