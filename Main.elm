module Main exposing (..)

import AnimationFrame
import Debug
import Html exposing (..)
import Html.Attributes exposing (width, height, style)
import Keyboard
import Models exposing (..)
import Msgs exposing (..)
import Time exposing (Time)
import Views
import GameLoop


-- INIT


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameOver then
        Sub.none
    else
        Sub.batch
            [ AnimationFrame.diffs OnAnimationFrame
            , Keyboard.downs OnKeyDown
            , Keyboard.ups OnKeyUp
            ]



-- VIEWS
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnKeyDown keyCode ->
            handleKeyDown model keyCode

        OnKeyUp keyCode ->
            handleKeyUp model keyCode

        OnAnimationFrame time ->
            GameLoop.updateAnimationFrame model time


handleKeyDown : Model -> Keyboard.KeyCode -> Return Msg
handleKeyDown model keyCode =
    let
        key =
            keyCodeToKey keyCode

        currentKeys =
            model.pressedKeys

        updatedKeys =
            case key of
                Up ->
                    { currentKeys | up = True }

                Down ->
                    { currentKeys | down = True }

                Left ->
                    { currentKeys | left = True }

                Right ->
                    { currentKeys | right = True }

                _ ->
                    currentKeys

        updatedModel =
            { model | pressedKeys = updatedKeys }
    in
        ( updatedModel, Cmd.none )
            |> tryShootBullet key


handleKeyUp : Model -> Keyboard.KeyCode -> ( Model, Cmd Msg )
handleKeyUp model keyCode =
    let
        key =
            keyCodeToKey keyCode

        currentKeys =
            model.pressedKeys

        updatedKeys =
            case key of
                Up ->
                    { currentKeys | up = False }

                Down ->
                    { currentKeys | down = False }

                Left ->
                    { currentKeys | left = False }

                Right ->
                    { currentKeys | right = False }

                _ ->
                    currentKeys

        updatedModel =
            { model | pressedKeys = updatedKeys }
    in
        ( updatedModel, Cmd.none )


tryShootBullet : Key -> Return Msg -> Return Msg
tryShootBullet key ( model, msg ) =
    if key == Space then
        if model.weaponCooldown == 0 then
            let
                (Ship shipPoint) =
                    model.playerShip

                friendlyBullets_ =
                    (Bullet shipPoint) :: model.friendlyBullets
            in
                ( { model | friendlyBullets = friendlyBullets_, weaponCooldown = weaponCooldownTime }, msg )
        else
            ( model, msg )
    else
        ( model, msg )



-- CURRENT FRAME
-- MAIN


main =
    Html.program
        { update = update
        , view = Views.view
        , subscriptions = subscriptions
        , init = init
        }
