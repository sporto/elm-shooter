module Main exposing (..)

import AnimationFrame
import Collision
import Debug
import Html exposing (..)
import Html.Attributes exposing (width, height, style)
import Keyboard
import Maybe.Extra
import Models exposing (..)
import Msgs exposing (..)
import Time exposing (Time)
import Views


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
            updateAnimationFrame model time


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
            if List.any (doesEnemyCollideWithBullet enemy) model.friendlyBullets then
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
        anyCollision =
            List.any
                (doesShipCollideWithEnemy model.playerShip)
                model.enemies
    in
        ( { model | gameOver = anyCollision }, Cmd.none )


updateNewEnemies : Time -> Return Msg -> Return Msg
updateNewEnemies diff ( model, msg ) =
    let
        difficulty =
            getDifficulty model

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



-- MAIN


main =
    Html.program
        { update = update
        , view = Views.view
        , subscriptions = subscriptions
        , init = init
        }



-- UTILS


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
