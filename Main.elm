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
import Text
import Time exposing (Time)


-- MODELS


type alias Point =
    ( Float, Float )


type Bullet
    = Bullet Point


type Ship
    = Ship Point


type alias Enemy =
    { createdTime : Time
    , position : Point
    }


type alias Model =
    { bullets : List Bullet
    , enemies : List Enemy
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
    150


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
    --let
    --    _ =
    --        Debug.log "bullets" model.bullets
    --in
    collage (truncate stageWidth)
        (truncate stageHeight)
        (drawActors model)
        |> Element.toHtml


drawActors : Model -> List Form
drawActors model =
    List.concat
        [ drawBgFar model
        , drawBgMedium model
        , [ drawPlayerShip model ]
        , drawEnemies model
        , drawBullets model
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


drawPlayerShip : Model -> Form
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


drawEnemies : Model -> List Form
drawEnemies model =
    List.map drawEnemy model.enemies


drawScore : Model -> List Form
drawScore model =
    let
        form =
            model.score
                |> toString
                |> Text.fromString
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


drawEnemy : Enemy -> Form
drawEnemy enemy =
    rect enemyWidth enemyHeight
        |> filled (Color.rgb 0 0 0)
        |> move enemy.position


bulletWidth =
    10


bulletHeight =
    4


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


updateEnemiesMovement : Time -> Return -> Return
updateEnemiesMovement diff ( model, msg ) =
    let
        moveEnemy : Enemy -> Enemy
        moveEnemy enemy =
            let
                ( x, y ) =
                    enemy.position

                position_ : Point
                position_ =
                    ( x - 1 |> max leftBoundary, y )
            in
                { enemy | position = position_ }

        enemies_ : List Enemy
        enemies_ =
            List.map moveEnemy model.enemies
    in
        ( { model | enemies = enemies_ }, msg )


updateEnemiesCollision : Time -> Return -> Return
updateEnemiesCollision diff ( model, msg ) =
    let
        checkEnemy : Enemy -> Maybe Enemy
        checkEnemy enemy =
            if List.any (doesEnemyCollideWithBullet enemy) model.bullets then
                Nothing
            else
                Just enemy

        standingEnemies =
            model.enemies
                |> List.filterMap checkEnemy

        score =
            List.length model.enemies - List.length standingEnemies
    in
        ( { model
            | enemies = standingEnemies
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
    if List.isEmpty model.enemies then
        -- Spawn
        ( { model | enemies = [ newEnemy model.time ] }, msg )
    else
        ( model, msg )



-- MAIN


main =
    Html.program
        { update = update
        , view = view
        , subscriptions = subscriptions
        , init = init
        }



-- UTILS


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
            x - bulletWidth / 2

        right =
            x + bulletWidth / 2

        top =
            y - bulletHeight / 2

        bottom =
            y + bulletHeight / 2
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
