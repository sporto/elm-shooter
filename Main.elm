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
    { coor : ( Float, Float )
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
    { coor = ( 0, 0 )
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
        [ player model
        ]
        |> Element.toHtml


player : Model -> Form
player model =
    rect 20 20
        |> filled (Color.rgb 0 0 0)
        |> move model.coor


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


movement =
    5


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        OnKeyboard keyCode ->
            let
                ( currentX, currentY ) =
                    model.coor

                newCoor =
                    case keyCodeToKey keyCode of
                        Up ->
                            ( currentX, currentY + movement )

                        Down ->
                            ( currentX, currentY - movement )

                        Left ->
                            ( currentX - movement, currentY )

                        Right ->
                            ( currentX + movement, currentY )

                        OtherKey ->
                            model.coor
            in
                ( { model | coor = newCoor }, Cmd.none )


main =
    Html.program
        { update = update
        , view = view
        , subscriptions = subscriptions
        , init = init
        }
