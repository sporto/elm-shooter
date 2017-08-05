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
import Time exposing (Time)


-- MODELS


type alias Point =
    ( Float, Float )


type Bullet
    = Bullet Point


type Ship
    = Ship Point


type Enemy
    = Enemy Point


type alias Model =
    { playerShip : Ship
    , pressedKeys :
        { up : Bool
        , down : Bool
        , left : Bool
        , right : Bool
        }
    , weaponCooldown : Float
    , bullets : List Bullet
    , enemies : List Enemy
    }


initialModel : Model
initialModel =
    { playerShip = Ship ( 0, 0 )
    , pressedKeys =
        { up = False
        , down = False
        , left = False
        , right = False
        }
    , weaponCooldown = 0
    , bullets = []
    , enemies = []
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


newEnemy : Enemy
newEnemy =
    Enemy ( rightBoundary, 0 )



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



-- INIT


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
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
        [ [ drawPlayerShip model ]
        , drawEnemies model
        , drawBullets model
        ]


drawPlayerShip : Model -> Form
drawPlayerShip model =
    let
        (Ship point) =
            model.playerShip
    in
        rect 20 20
            |> filled (Color.rgb 0 0 0)
            |> move point


drawEnemies : Model -> List Form
drawEnemies model =
    List.map drawEnemy model.enemies


drawEnemy : Enemy -> Form
drawEnemy (Enemy point) =
    rect 12 12
        |> filled (Color.rgb 0 0 0)
        |> move point


drawBullet : Bullet -> Form
drawBullet (Bullet point) =
    rect 10 4
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


handleKeyDown : Model -> Keyboard.KeyCode -> ( Model, Cmd msg )
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


tryShootBullet : Key -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
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


updateAnimationFrame : Model -> Time -> ( Model, Cmd msg )
updateAnimationFrame model diff =
    ( model, Cmd.none )
        |> updateShipMovement diff
        |> updateBulletsMovement diff
        |> updateWeaponCooldown diff
        |> updateEnemiesMovement diff
        |> updateEnemiesCollision diff
        |> updateNewEnemies diff


updateShipMovement : Time -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
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


updateBulletsMovement : Time -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
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


updateWeaponCooldown : Time -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateWeaponCooldown diff ( model, msg ) =
    let
        weaponCooldown_ =
            model.weaponCooldown
                |> (\current -> current - weaponCooldownForDiff diff)
                |> max 0
    in
        ( { model | weaponCooldown = weaponCooldown_ }, msg )


updateEnemiesMovement : Time -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateEnemiesMovement diff ( model, msg ) =
    let
        moveEnemy (Enemy ( x, y )) =
            Enemy ( x - 1 |> max leftBoundary, y )

        enemies_ =
            List.map moveEnemy model.enemies
    in
        ( { model | enemies = enemies_ }, msg )


updateEnemiesCollision diff ( model, msg ) =
    let
        checkEnemy enemy =
            if List.any (doesEnemyCollideWithBullet enemy) model.bullets then
                Nothing
            else
                Just enemy

        standingEnemies =
            model.enemies
                |> List.filterMap checkEnemy
    in
        ( { model | enemies = standingEnemies }, msg )


updateNewEnemies : Time -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateNewEnemies diff ( model, msg ) =
    if List.isEmpty model.enemies then
        -- Spawn
        ( { model | enemies = [ newEnemy ] }, msg )
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


doesEnemyCollideWithBullet : Enemy -> Bullet -> Bool
doesEnemyCollideWithBullet enemy bullet =
    False



--Collision.collision 2 () ()
--    |> Maybe.withDefault False
