module GameLoop exposing (..)

import Audio
import Bullet
import Constants exposing (..)
import List.Extra
import Maybe.Extra
import Models exposing (..)
import Msgs exposing (..)
import Position
import Time exposing (Time)
import Utils


updateAnimationFrame : Model -> Time -> Return Msg
updateAnimationFrame model diff =
    ( model, Cmd.none )
        |> updateStage diff
        -- Collisions
        |> updateEnemiesCollision diff
        |> updateShipCollision diff
        |> updateShipPowerUpCollision diff
        -- Actions
        |> updatePowerUp diff
        |> updateShip diff
        |> updateEnemiesMovementAndCooldown diff
        |> updateEnemiesShots diff
        |> updateFriendlyBullets diff
        |> updateEnemyBullets diff
        -- Other
        |> updateNewEnemies diff
        |> updateExplosions diff


updateStage : Time -> Return Msg -> Return Msg
updateStage diff ( model, cmd ) =
    let
        scrollX_ =
            model.time + diff
    in
        ( { model | time = scrollX_ }, cmd )


updatePowerUp : Time -> Return Msg -> Return Msg
updatePowerUp diff ( model, cmd ) =
    case model.powerUp of
        Nothing ->
            let
                nextPowerUpTime =
                    (toFloat model.powerUpCount) * powerUpFrequency + initialPowerUpDelay

                shouldSpawn =
                    (model.time > nextPowerUpTime)
                        && (model.level < maxLevel)
            in
                if shouldSpawn then
                    let
                        powerUp =
                            { position = ( rightBoundary, 32 )
                            }
                    in
                        ( { model
                            | powerUp = Just powerUp
                            , powerUpCount = model.powerUpCount + 1
                          }
                        , cmd
                        )
                else
                    ( model, cmd )

        Just pu ->
            -- Move the current power up
            let
                ( x, y ) =
                    pu.position

                position_ =
                    ( x - powerUpMovementForDiff diff, y )

                powerUp_ =
                    if Position.isInStage pu.position then
                        Just { pu | position = position_ }
                    else
                        Nothing
            in
                ( { model | powerUp = powerUp_ }, cmd )


updateShipMovement : Model -> Time -> Ship -> Ship
updateShipMovement model diff (Ship playerShipPoint) =
    let
        movement =
            shipMovementForDiff diff

        plusUp ( x, y ) =
            if model.pressedKeys.up then
                ( x, y + movement )
            else
                ( x, y )

        plusDown ( x, y ) =
            if model.pressedKeys.down then
                ( x, y - movement )
            else
                ( x, y )

        plusLeft ( x, y ) =
            if model.pressedKeys.left then
                ( x - movement, y )
            else
                ( x, y )

        plusRight ( x, y ) =
            if model.pressedKeys.right then
                ( x + movement, y )
            else
                ( x, y )

        bound ( x, y ) =
            ( x
                |> min (rightBoundary - 16)
                |> max (leftBoundary + 16)
            , y
                |> min (bottomBoundary - 32)
                |> max (topBoundary + 32)
            )

        newCoor =
            playerShipPoint
                |> plusUp
                |> plusDown
                |> plusLeft
                |> plusRight
                |> bound
    in
        Ship newCoor


updateShip : Time -> Return Msg -> Return Msg
updateShip diff ( model, cmd ) =
    let
        ship_ =
            updateShipMovement model diff model.playerShip

        weaponCooldown_ =
            model.weaponCooldown
                |> (\current -> current - weaponCooldownForDiff diff)
                |> max 0

        respawnIn_ =
            max 0 (model.respawnIn - diff)
    in
        ( { model
            | playerShip = ship_
            , weaponCooldown = weaponCooldown_
            , respawnIn = respawnIn_
          }
        , cmd
        )


updateFriendlyBullets : Time -> Return Msg -> Return Msg
updateFriendlyBullets diff ( model, cmd ) =
    let
        movedBullets =
            model.friendlyBullets
                |> List.map (Bullet.move diff)
                |> List.filter Bullet.isInStage
    in
        ( { model | friendlyBullets = movedBullets }, cmd )


updateEnemyBullets : Time -> Return Msg -> Return Msg
updateEnemyBullets diff ( model, cmd ) =
    let
        enemyBullets_ : List Bullet
        enemyBullets_ =
            model.enemyBullets
                |> List.map (Bullet.move diff)
                |> List.filter Bullet.isInStage
    in
        ( { model | enemyBullets = enemyBullets_ }, cmd )


updateEnemyPosition : Time -> Time -> Enemy -> Enemy
updateEnemyPosition totalTime diff enemy =
    let
        relativeTime =
            totalTime - enemy.createdTime

        timeVar =
            relativeTime / 900

        ( x, y ) =
            enemy.position

        increase =
            pi * 2 / 100

        newY =
            (sin timeVar / 2) * (stageHeight * 0.8)

        x_ =
            if enemy.direction == DirectionLeft then
                x - 1
            else
                x + 1

        position_ : Point
        position_ =
            ( x_, newY )

        direction_ =
            if x_ < leftBoundary then
                DirectionRight
            else if x_ > rightBoundary then
                DirectionLeft
            else
                enemy.direction
    in
        { enemy | position = position_, direction = direction_ }


updateEnemiesMovementAndCooldown : Time -> Return Msg -> Return Msg
updateEnemiesMovementAndCooldown diff ( model, cmd ) =
    let
        updateWeaponCooldown : Enemy -> Enemy
        updateWeaponCooldown enemy =
            { enemy | weaponCooldown = enemy.weaponCooldown - diff }

        enemies_ : List Enemy
        enemies_ =
            model.enemies
                |> List.map (updateEnemyPosition model.time diff)
                |> List.map updateWeaponCooldown
    in
        ( { model | enemies = enemies_ }, cmd )


updateEnemiesShots : Time -> Return Msg -> Return Msg
updateEnemiesShots diff ( model, cmd ) =
    let
        getShipY (Ship (_, y)) =
            y

        shipY =
            getShipY model.playerShip

        attemptShot : Enemy -> ( Enemy, Maybe Bullet )
        attemptShot enemy =
            let
                (_, y ) =
                    enemy.position

                shipIsInRange  =
                    y > (shipY - 12 ) && y < (shipY + 12)
            in   
                if enemy.weaponCooldown <= 0 && shipIsInRange then
                    ( { enemy | weaponCooldown = enemyWeaponCooldownTime }
                    , Just
                        { position = enemy.position
                        , direction = enemy.direction
                        , speed = enemyBulletSpeed
                        }
                    )
                else
                    ( enemy, Nothing )

        updates =
            model.enemies
                |> List.map attemptShot

        newBullets : List Bullet
        newBullets =
            List.filterMap Tuple.second updates

        enemyBullets_ : List Bullet
        enemyBullets_ =
            newBullets ++ model.enemyBullets

        enemies_ : List Enemy
        enemies_ =
            List.map Tuple.first updates
    in
        ( { model
            | enemyBullets = enemyBullets_
            , enemies = enemies_
          }
        , cmd
        )


updateEnemiesCollision : Time -> Return Msg -> Return Msg
updateEnemiesCollision diff ( model, cmd ) =
    let
        checkEnemy : Enemy -> Maybe ( Enemy, Bullet, Explosion )
        checkEnemy enemy =
            let
                maybeBullet =
                    List.Extra.find (Utils.doesEnemyCollideWithBullet enemy) model.friendlyBullets

                explosion =
                    { position = enemy.position
                    , time = 0
                    }
            in
                case maybeBullet of
                    Just bullet ->
                        Just ( enemy, bullet, explosion )

                    Nothing ->
                        Nothing

        collisionResults : List ( Enemy, Bullet, Explosion )
        collisionResults =
            model.enemies
                |> List.filterMap checkEnemy

        enemiesKilled : List Enemy
        enemiesKilled =
            collisionResults
                |> List.map (\( e, _, _ ) -> e)

        newExplosions : List Explosion
        newExplosions =
            collisionResults
                |> List.map (\( _, _, ex ) -> ex)

        bulletsUsed : List Bullet
        bulletsUsed =
            collisionResults
                |> List.map (\( _, b, _ ) -> b)

        score =
            List.length newExplosions

        enemies_ : List Enemy
        enemies_ =
            model.enemies
                |> List.Extra.filterNot
                    (\enemy ->
                        List.member enemy enemiesKilled
                    )

        explosions_ =
            List.concat [ newExplosions, model.explosions ]

        friendlyBullets_ =
            model.friendlyBullets
                |> List.Extra.filterNot
                    (\b ->
                        List.member b bulletsUsed
                    )

        newCmd =
            if newExplosions == [] then
                Cmd.none
            else
                Audio.playExplosion
    in
        ( { model
            | enemies = enemies_
            , explosions = explosions_
            , score = model.score + score
            , friendlyBullets = friendlyBullets_
          }
        , Cmd.batch [ cmd, newCmd ]
        )


updateShipCollision : Time -> Return Msg -> Return Msg
updateShipCollision diff ( model, cmd ) =
    let
        maybeEnemy =
            List.filter
                (Utils.doesShipCollideWithEnemy model.playerShip)
                model.enemies
                |> List.head

        maybeBullet =
            List.filter
                (Utils.doesShipCollideWithEnemyBullet model.playerShip)
                model.enemyBullets
                |> List.head

        -- Remove collided enemies or bullets
        enemies_ =
            case maybeEnemy of
                Just enemy ->
                    model.enemies
                        |> List.filter (\e -> e /= enemy)

                Nothing ->
                    model.enemies

        enemyBullets_ =
            case maybeBullet of
                Just bullet ->
                    model.enemyBullets
                        |> List.filter (\b -> b /= bullet)

                Nothing ->
                    model.enemyBullets

        anyCollision =
            Maybe.Extra.isJust maybeEnemy || Maybe.Extra.isJust maybeBullet

        ( lifes_, respawnIn_, newCmd ) =
            if anyCollision then
                ( model.lifes - 1, respawnTime, Audio.playExplosionBig )
            else
                ( model.lifes, model.respawnIn, Cmd.none )

        cmd_ =
            Cmd.batch [ cmd, newCmd ]
    in
        if Utils.isInvincible model then
            ( model, cmd )
        else
            ( { model
                | lifes = lifes_
                , respawnIn = respawnIn_
                , enemies = enemies_
                , enemyBullets = enemyBullets_
              }
            , cmd_
            )


updateShipPowerUpCollision : Time -> Return Msg -> Return Msg
updateShipPowerUpCollision diff ( model, cmd ) =
    case model.powerUp of
        Nothing ->
            ( model, cmd )

        Just pu ->
            let
                collision =
                    Utils.doesShipCollideWithPowerUp model.playerShip pu
            in
                if collision then
                    ( { model
                        | powerUp = Nothing
                        , level = model.level + 1
                      }
                    , cmd
                    )
                else
                    ( model, cmd )


updateNewEnemies : Time -> Return Msg -> Return Msg
updateNewEnemies diff ( model, cmd ) =
    let
        difficulty =
            Utils.getDifficulty model

        timeToNextSpawn =
            5 * Time.second / difficulty

        spawnIfOld enemy =
            let
                timeAlife =
                    model.time - enemy.createdTime
            in
                if timeAlife > timeToNextSpawn then
                    True
                else
                    False

        shouldSpawn =
            List.head model.enemies
                |> Maybe.map spawnIfOld
                |> Maybe.withDefault True

        enemies_ =
            if shouldSpawn then
                newEnemy model.time :: model.enemies
            else
                model.enemies
    in
        if shouldSpawn then
            -- Spawn
            ( { model | enemies = enemies_ }, cmd )
        else
            ( model, cmd )


updateExplosions : Time -> Return Msg -> Return Msg
updateExplosions diff ( model, cmd ) =
    let
        updateExplotion : Explosion -> Maybe Explosion
        updateExplotion explosion =
            if explosion.time > 1 * Time.second then
                Nothing
            else
                Just { explosion | time = explosion.time + diff }

        explosions_ =
            List.filterMap updateExplotion model.explosions
    in
        ( { model | explosions = explosions_ }, cmd )
