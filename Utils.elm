module Utils exposing (..)

import Collision
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
            x - shipWidth / 2

        right =
            x + shipWidth / 2

        top =
            y - shipHeight / 2

        bottom =
            y + shipHeight / 2
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
            x - enemyWidth / 2

        right =
            x + enemyWidth / 2

        top =
            y - enemyHeight / 2

        bottom =
            y + enemyHeight / 2
    in
        [ ( top, left )
        , ( top, right )
        , ( bottom, right )
        , ( bottom, left )
        ]


getEnemyBulletBoundingBox : EnemyBullet -> List Point
getEnemyBulletBoundingBox bullet =
    let
        ( x, y ) =
            bullet.position

        left =
            x - enemyBulletWidth / 2

        right =
            x + enemyBulletWidth / 2

        top =
            y - enemyBulletHeight / 2

        bottom =
            y + enemyBulletHeight / 2
    in
        [ ( top, left )
        , ( top, right )
        , ( bottom, right )
        , ( bottom, left )
        ]


getBulletBoundingBox (Bullet ( x, y )) =
    let
        left =
            x - bulletWidth / 2 - 4

        right =
            x + bulletWidth / 2 + 4

        top =
            y - bulletHeight / 2 - 4

        bottom =
            y + bulletHeight / 2 + 4
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
            getBulletBoundingBox bullet
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


doesShipCollideWithEnemyBullet : Ship -> EnemyBullet -> Bool
doesShipCollideWithEnemyBullet ship bullet =
    let
        bulletPolly =
            getEnemyBulletBoundingBox bullet

        shipPolly =
            getShipBoundingBox ship
    in
        Collision.collision 2 ( bulletPolly, polySupport ) ( shipPolly, polySupport )
            |> Maybe.withDefault False



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
