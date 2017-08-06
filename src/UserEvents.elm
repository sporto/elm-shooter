module UserEvents exposing (..)

import Audio
import Keyboard
import Models exposing (..)
import Msgs exposing (..)
import Utils


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
    if key == Space && Utils.canShoot model then
        if model.weaponCooldown == 0 then
            let
                (Ship shipPoint) =
                    model.playerShip

                newBullet =
                    { position = shipPoint
                    , direction = DirectionRight
                    }

                friendlyBullets_ =
                    newBullet :: model.friendlyBullets
            in
                ( { model | friendlyBullets = friendlyBullets_, weaponCooldown = weaponCooldownTime }
                , Audio.playLaser
                )
        else
            ( model, msg )
    else
        ( model, msg )
