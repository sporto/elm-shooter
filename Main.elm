module Main exposing (..)

import AnimationFrame
import Collage exposing (..)
import Collision
import Color
import Debug
import Element
import Html exposing (..)
import Html.Attributes exposing (width, height, style)
import Keyboard
import Maybe.Extra
import Text
import Time exposing (Time)


-- MODELS


type Direction
    = DirectionLeft
    | DirectionRight


type alias Point =
    ( Float, Float )


type Bullet
    = Bullet Point


type Ship
    = Ship Point


type alias Explosion =
    { position : Point
    , time : Time
    }


type alias Enemy =
    { createdTime : Time
    , position : Point
    , direction : Direction
    }


type alias Model =
    { bullets : List Bullet
    , enemies : List Enemy
    , explosions : List Explosion
    , gameOver : Bool
    , playerShip : Ship
    , pressedKeys :
        { up : Bool
        , down : Bool
        , left : Bool
        , right : Bool
        }
    , score : Int
    , time : Float
    , weaponCooldown : Float
    }


initialModel : Model
initialModel =
    { bullets = []
    , enemies = []
    , explosions = []
    , gameOver = False
    , playerShip = Ship ( 0, 0 )
    , pressedKeys =
        { up = False
        , down = False
        , left = False
        , right = False
        }
    , score = 0
    , time = 0
    , weaponCooldown = 0
    }


type Key
    = Up
    | Down
    | Left
    | Right
    | Space
    | OtherKey


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

        32 ->
            Space

        _ ->
            OtherKey


newEnemy : Time -> Enemy
newEnemy time =
    { createdTime = time
    , direction = DirectionLeft
    , position = ( rightBoundary, 0 )
    }


type alias Return =
    ( Model, Cmd Msg )



-- MSGs


type Msg
    = NoOp
    | OnKeyDown Keyboard.KeyCode
    | OnKeyUp Keyboard.KeyCode
    | OnAnimationFrame Time



-- CONSTANTS


stageHeight : Float
stageHeight =
    400


stageWidth : Float
stageWidth =
    1200


topBoundary : Float
topBoundary =
    bottomBoundary * -1


bottomBoundary : Float
bottomBoundary =
    stageHeight / 2


leftBoundary : Float
leftBoundary =
    rightBoundary * -1


rightBoundary : Float
rightBoundary =
    stageWidth / 2


weaponCooldownTime =
    0.5 * Time.second


shipMovementForDiff diff =
    diff / 2


bulletMovementForDiff diff =
    diff


weaponCooldownForDiff diff =
    diff


bgMovementForDiff : Float -> Float -> Float
bgMovementForDiff time distance =
    time * -1 / distance



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


view : Model -> Html msg
view model =
    collage (truncate stageWidth)
        (truncate stageHeight)
        (drawActors model)
        |> Element.toHtml


drawActors : Model -> List Form
drawActors model =
    List.concat
        [ -- Background
          drawBgFar model
        , drawBgMedium model

        -- Ships
        , drawPlayerShip model
        , drawEnemies model
        , drawBullets model
        , drawExplosions model

        -- UI
        , drawScore model
        , drawGameOver model
        ]


drawBg : Model -> String -> Float -> List Form
drawBg model image distance =
    let
        w =
            round stageWidth

        h =
            round stageHeight

        bg =
            Element.image w h image
                |> toForm

        x =
            toFloat (round (bgMovementForDiff model.time distance) % round stageWidth)
    in
        [ bg
            |> moveX x
        , bg
            |> moveX (x - stageWidth)
        ]


drawBgFar : Model -> List Form
drawBgFar model =
    drawBg model "assets/bg-clouds.png" 40


drawBgMedium : Model -> List Form
drawBgMedium model =
    drawBg model "assets/bg-hills.png" 10


shipWidth =
    20


shipHeight =
    20


drawPlayerShip : Model -> List Form
drawPlayerShip model =
    let
        (Ship point) =
            model.playerShip

        file =
            "assets/ship.png"
    in
        Element.image shipWidth shipHeight file
            |> toForm
            |> move point
            |> List.singleton


drawEnemy : Enemy -> Form
drawEnemy enemy =
    let
        file =
            "assets/enemy-1.png"
    in
        Element.image enemyWidth enemyHeight file
            |> toForm
            |> move enemy.position


drawEnemies : Model -> List Form
drawEnemies model =
    List.map drawEnemy model.enemies


drawExplosion : Explosion -> Form
drawExplosion explosion =
    let
        file =
            "assets/explosion.png"
    in
        Element.image explosionWidth explosionHeight file
            |> toForm
            |> move explosion.position


drawExplosions : Model -> List Form
drawExplosions model =
    List.map drawExplosion model.explosions


drawScore : Model -> List Form
drawScore model =
    let
        form =
            model.score
                |> toString
                |> Text.fromString
                |> Text.bold
                |> Text.height 16
                |> Collage.text
                |> move ( leftBoundary + 20, bottomBoundary - 20 )
    in
        [ form
        ]


drawGameOver : Model -> List Form
drawGameOver model =
    let
        form =
            "Game Over"
                |> Text.fromString
                |> Text.height 50
                |> Text.bold
                |> Text.color Color.red
                |> Collage.text
    in
        if model.gameOver then
            [ form ]
        else
            []


enemyWidth =
    12


enemyHeight =
    12


bulletWidth =
    10


bulletHeight =
    4


explosionWidth =
    20


explosionHeight =
    20


drawBullet : Bullet -> Form
drawBullet (Bullet point) =
    rect bulletWidth bulletHeight
        |> filled (Color.rgb 0 0 0)
        |> move point


drawBullets : Model -> List Form
drawBullets model =
    List.map drawBullet model.bullets



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


handleKeyDown : Model -> Keyboard.KeyCode -> Return
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


tryShootBullet : Key -> Return -> Return
tryShootBullet key ( model, msg ) =
    if key == Space then
        if model.weaponCooldown == 0 then
            let
                (Ship shipPoint) =
                    model.playerShip

                bullets_ =
                    (Bullet shipPoint) :: model.bullets
            in
                ( { model | bullets = bullets_, weaponCooldown = weaponCooldownTime }, msg )
        else
            ( model, msg )
    else
        ( model, msg )



-- CURRENT FRAME


updateAnimationFrame : Model -> Time -> Return
updateAnimationFrame model diff =
    ( model, Cmd.none )
        |> updateStage diff
        -- Movements
        |> updateShipMovement diff
        |> updateBulletsMovement diff
        |> updateEnemiesMovement diff
        -- Collisions
        |> updateEnemiesCollision diff
        |> updateShipCollision diff
        -- Other
        |> updateWeaponCooldown diff
        |> updateNewEnemies diff
        |> updateExplotions diff


updateStage : Time -> Return -> Return
updateStage diff ( model, msg ) =
    let
        scrollX_ =
            model.time + diff
    in
        ( { model | time = scrollX_ }, msg )


updateShipMovement : Time -> Return -> Return
updateShipMovement diff ( model, msg ) =
    let
        (Ship playerShipPoint) =
            model.playerShip

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
        ( { model | playerShip = Ship newCoor }, msg )


updateBulletsMovement : Time -> Return -> Return
updateBulletsMovement diff ( model, msg ) =
    let
        movement =
            bulletMovementForDiff diff

        moveBullet (Bullet ( x, y )) =
            Bullet ( x + movement, y )

        isBulletInStage (Bullet ( x, y )) =
            x < rightBoundary

        movedBullets =
            model.bullets
                |> List.map moveBullet
                |> List.filter isBulletInStage
    in
        ( { model | bullets = movedBullets }, msg )


updateWeaponCooldown : Time -> Return -> Return
updateWeaponCooldown diff ( model, msg ) =
    let
        weaponCooldown_ =
            model.weaponCooldown
                |> (\current -> current - weaponCooldownForDiff diff)
                |> max 0
    in
        ( { model | weaponCooldown = weaponCooldown_ }, msg )


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


updateEnemiesMovement : Time -> Return -> Return
updateEnemiesMovement diff ( model, msg ) =
    let
        enemies_ : List Enemy
        enemies_ =
            List.map (updateEnemyPosition model.time diff) model.enemies
    in
        ( { model | enemies = enemies_ }, msg )


updateEnemiesCollision : Time -> Return -> Return
updateEnemiesCollision diff ( model, msg ) =
    let
        checkEnemy : Enemy -> ( Maybe Enemy, Maybe Explosion )
        checkEnemy enemy =
            if List.any (doesEnemyCollideWithBullet enemy) model.bullets then
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


updateShipCollision : Time -> Return -> Return
updateShipCollision diff ( model, msg ) =
    let
        anyCollision =
            List.any
                (doesShipCollideWithEnemy model.playerShip)
                model.enemies
    in
        ( { model | gameOver = anyCollision }, Cmd.none )


updateNewEnemies : Time -> Return -> Return
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


updateExplotions : Time -> Return -> Return
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
        , view = view
        , subscriptions = subscriptions
        , init = init
        }



-- UTILS


getDifficulty : Model -> Float
getDifficulty model =
    let
        difficulty =
            0.5 + model.time / (20 * Time.second)
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
