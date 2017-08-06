module Utils exposing (..)

import Collision
import Constants exposing (..)
import Models exposing (..)
import Time exposing (Time)


getDifficulty : Model -> Float
getDifficulty model =
    let
        startingDifficulty =
            0.5

        secondsToIncrease1Level =
            30 * Time.second

        difficulty =
            model.time / secondsToIncrease1Level + startingDifficulty
    in
        difficulty


getShipBoundingBox : Ship -> List Point
getShipBoundingBox (Ship ( x, y )) =
    let
        left =
            x - (toFloat shipWidth) / 2

        right =
            x + (toFloat shipWidth) / 2

        top =
            y - (toFloat shipHeight) / 2

        bottom =
            y + (toFloat shipHeight) / 2
    in
        [ ( top, left )
        , ( top, right )
        , ( bottom, right )
        , ( bottom, left )
        ]


getEnemyBoundingBox : Enemy -> List Point
getEnemyBoundingBox enemy =
    let
        ( x, y ) =
            enemy.position

        left =
            x - (toFloat enemyWidth) / 2

        right =
            x + (toFloat enemyWidth) / 2

        top =
            y - (toFloat enemyHeight) / 2

        bottom =
            y + (toFloat enemyHeight) / 2
    in
        [ ( top, left )
        , ( top, right )
        , ( bottom, right )
        , ( bottom, left )
        ]


getEnemyBulletBoundingBox : Bullet -> List Point
getEnemyBulletBoundingBox bullet =
    let
        ( x, y ) =
            bullet.position

        left =
            x - (toFloat enemyBulletWidth) / 2

        right =
            x + (toFloat enemyBulletWidth) / 2

        top =
            y - (toFloat enemyBulletHeight) / 2

        bottom =
            y + (toFloat enemyBulletHeight) / 2
    in
        [ ( top, left )
        , ( top, right )
        , ( bottom, right )
        , ( bottom, left )
        ]


getFriendlyBulletBoundingBox : Bullet -> List Point
getFriendlyBulletBoundingBox bullet =
    let
        ( x, y ) =
            bullet.position

        left =
            x - (toFloat bulletWidth) / 2 - 4

        right =
            x + (toFloat bulletWidth) / 2 + 4

        top =
            y - (toFloat bulletHeight) / 2 - 4

        bottom =
            y + (toFloat bulletHeight) / 2 + 4
    in
        [ ( top, left )
        , ( top, right )
        , ( bottom, right )
        , ( bottom, left )
        ]


getPowerUpBoundingBox : PowerUp -> List Point
getPowerUpBoundingBox powerUp =
    let
        ( x, y ) =
            powerUp.position

        ( _, w, h ) =
            powerUpAsset

        left =
            x - (toFloat w) / 2 - 4

        right =
            x + (toFloat w) / 2 + 4

        top =
            y - (toFloat h) / 2 - 4

        bottom =
            y + (toFloat h) / 2 + 4
    in
        [ ( top, left )
        , ( top, right )
        , ( bottom, right )
        , ( bottom, left )
        ]


doesEnemyCollideWithBullet : Enemy -> Bullet -> Bool
doesEnemyCollideWithBullet enemy bullet =
    let
        enemyPolly =
            getEnemyBoundingBox enemy

        bulletPolly =
            getFriendlyBulletBoundingBox bullet
    in
        Collision.collision 2 ( enemyPolly, polySupport ) ( bulletPolly, polySupport )
            |> Maybe.withDefault False


doesShipCollideWithEnemy : Ship -> Enemy -> Bool
doesShipCollideWithEnemy ship enemy =
    let
        enemyPolly =
            getEnemyBoundingBox enemy

        shipPolly =
            getShipBoundingBox ship
    in
        Collision.collision 2 ( enemyPolly, polySupport ) ( shipPolly, polySupport )
            |> Maybe.withDefault False


doesShipCollideWithEnemyBullet : Ship -> Bullet -> Bool
doesShipCollideWithEnemyBullet ship bullet =
    let
        bulletPolly =
            getEnemyBulletBoundingBox bullet

        shipPolly =
            getShipBoundingBox ship
    in
        Collision.collision 2 ( bulletPolly, polySupport ) ( shipPolly, polySupport )
            |> Maybe.withDefault False


doesShipCollideWithPowerUp : Ship -> PowerUp -> Bool
doesShipCollideWithPowerUp ship pu =
    let
        powerUpPolly =
            getPowerUpBoundingBox pu

        shipPolly =
            getShipBoundingBox ship
    in
        Collision.collision 2 ( powerUpPolly, polySupport ) ( shipPolly, polySupport )
            |> Maybe.withDefault False


canShoot : Model -> Bool
canShoot =
    isRespawning >> not


isRespawning : Model -> Bool
isRespawning model =
    model.respawnIn > 0


isInvincible : Model -> Bool
isInvincible =
    isRespawning



---


dot : Collision.Pt -> Collision.Pt -> Float
dot ( x1, y1 ) ( x2, y2 ) =
    (x1 * x2) + (y1 * y2)


polySupport : List Collision.Pt -> Collision.Pt -> Maybe Collision.Pt
polySupport list d =
    let
        dotList =
            List.map (dot d) list

        decorated =
            (List.map2 (,)) dotList list

        max =
            List.maximum decorated
    in
        case max of
            Just ( m, p ) ->
                Just p

            _ ->
                Nothing
