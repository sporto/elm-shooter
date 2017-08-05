module GameLoop exposing (..)

import Maybe.Extra
import Models exposing (..)
import Msgs exposing (..)
import Time exposing (Time)
import Utils


updateAnimationFrame : Model -> Time -> Return Msg
updateAnimationFrame model diff =
    ( model, Cmd.none )
        |> updateStage diff
        -- Collisions
        |> updateEnemiesCollision diff
        |> updateShipCollision diff
        -- Actions
        |> updateShip diff
        |> updateEnemiesMovementAndCooldown diff
        |> updateEnemiesShots diff
        |> updateFriendlyBullets diff
        |> updateEnemyBullets diff
        -- Other
        |> updateNewEnemies diff
        |> updateExplotions diff


updateStage : Time -> Return Msg -> Return Msg
updateStage diff ( model, msg ) =
    let
        scrollX_ =
            model.time + diff
    in
        ( { model | time = scrollX_ }, msg )


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
                |> min (rightBoundary - 10)
                |> max (leftBoundary + 10)
            , y
                |> min (bottomBoundary - 10)
                |> max (topBoundary + 10)
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
updateShip diff ( model, msg ) =
    let
        ship_ =
            updateShipMovement model diff model.playerShip

        weaponCooldown_ =
            model.weaponCooldown
                |> (\current -> current - weaponCooldownForDiff diff)
                |> max 0
    in
        ( { model
            | playerShip = ship_
            , weaponCooldown = weaponCooldown_
          }
        , msg
        )


updateFriendlyBullets : Time -> Return Msg -> Return Msg
updateFriendlyBullets diff ( model, msg ) =
    let
        movement =
            bulletMovementForDiff diff

        moveBullet (Bullet ( x, y )) =
            Bullet ( x + movement, y )

        isBulletInStage (Bullet ( x, y )) =
            x < rightBoundary

        movedBullets =
            model.friendlyBullets
                |> List.map moveBullet
                |> List.filter isBulletInStage
    in
        ( { model | friendlyBullets = movedBullets }, msg )


updateEnemyBullets : Time -> Return Msg -> Return Msg
updateEnemyBullets diff ( model, msg ) =
    let
        movement =
            enemyBulletMovementForDiff diff

        moveBullet : EnemyBullet -> EnemyBullet
        moveBullet bullet =
            let
                ( x, y ) =
                    bullet.position

                x_ =
                    if bullet.direction == DirectionLeft then
                        x - movement
                    else
                        x + movement

                position_ =
                    ( x_, y )
            in
                { bullet | position = position_ }

        inStage : EnemyBullet -> Bool
        inStage { position } =
            let
                ( x, y ) =
                    position
            in
                x > leftBoundary && x < rightBoundary

        enemyBullets_ : List EnemyBullet
        enemyBullets_ =
            model.enemyBullets
                |> List.map moveBullet
                |> List.filter inStage
    in
        ( { model | enemyBullets = enemyBullets_ }, msg )


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
updateEnemiesMovementAndCooldown diff ( model, msg ) =
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
        ( { model | enemies = enemies_ }, msg )


updateEnemiesShots : Time -> Return Msg -> Return Msg
updateEnemiesShots diff ( model, msg ) =
    let
        attemptShot : Enemy -> ( Enemy, Maybe EnemyBullet )
        attemptShot enemy =
            if enemy.weaponCooldown <= 0 then
                ( { enemy | weaponCooldown = enemyWeaponCooldownTime }
                , Just
                    { position = enemy.position
                    , direction = enemy.direction
                    }
                )
            else
                ( enemy, Nothing )

        updates =
            model.enemies
                |> List.map attemptShot

        newBullets : List EnemyBullet
        newBullets =
            List.filterMap Tuple.second updates

        enemyBullets_ : List EnemyBullet
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
        , msg
        )


updateEnemiesCollision : Time -> Return Msg -> Return Msg
updateEnemiesCollision diff ( model, msg ) =
    let
        checkEnemy : Enemy -> ( Maybe Enemy, Maybe Explosion )
        checkEnemy enemy =
            if List.any (Utils.doesEnemyCollideWithBullet enemy) model.friendlyBullets then
                let
                    explosion =
                        { position = enemy.position
                        , time = 0
                        }
                in
                    ( Nothing, Just explosion )
            else
                ( Just enemy, Nothing )

        enemiesOrExplosions =
            model.enemies
                |> List.map checkEnemy

        enemies_ : List Enemy
        enemies_ =
            enemiesOrExplosions
                |> List.map Tuple.first
                |> Maybe.Extra.values

        newExplosions : List Explosion
        newExplosions =
            enemiesOrExplosions
                |> List.map Tuple.second
                |> Maybe.Extra.values

        score =
            List.length newExplosions

        explosions_ =
            List.concat [ newExplosions, model.explosions ]
    in
        ( { model
            | enemies = enemies_
            , explosions = explosions_
            , score = model.score + score
          }
        , msg
        )


updateShipCollision : Time -> Return Msg -> Return Msg
updateShipCollision diff ( model, msg ) =
    let
        collisionWithEnemy =
            List.any
                (Utils.doesShipCollideWithEnemy model.playerShip)
                model.enemies

        collisionWithEnemyBullet =
            List.any
                (Utils.doesShipCollideWithEnemyBullet model.playerShip)
                model.enemyBullets

        anyCollision =
            collisionWithEnemy || collisionWithEnemyBullet
    in
        ( { model | gameOver = anyCollision }, Cmd.none )


updateNewEnemies : Time -> Return Msg -> Return Msg
updateNewEnemies diff ( model, msg ) =
    let
        difficulty =
            Utils.getDifficulty model

        timeToNextSpawn =
            5 * Time.second / difficulty

        spawnIfOld enemy =
            let
                timeAlive =
                    model.time - enemy.createdTime
            in
                if timeAlive > timeToNextSpawn then
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
            ( { model | enemies = enemies_ }, msg )
        else
            ( model, msg )


updateExplotions : Time -> Return Msg -> Return Msg
updateExplotions diff ( model, msg ) =
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
        ( { model | explosions = explosions_ }, msg )
