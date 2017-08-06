module Views exposing (..)

import Collage exposing (..)
import Color
import Element
import Html exposing (..)
import Html.Attributes exposing (style)
import Models exposing (..)
import Text
import Time exposing (Time)


--import Transform


view : Model -> Html msg
view model =
    let
        a =
            1

        --_ =
        --    Debug.log "respawnIn" model.respawnIn
    in
        div [ style [ ( "margin", "0 auto" ), ( "width", "1200px" ) ] ]
            [ collage (truncate stageWidth)
                (truncate stageHeight)
                (drawActors model)
                |> Element.toHtml
            ]


drawActors : Model -> List Form
drawActors model =
    List.concat
        [ -- Background
          drawBgFar model
        , drawBgMedium model
        , drawBgNear model

        -- Ships
        , drawPowerUps model
        , drawShip model
        , drawEnemies model
        , drawBullets model
        , drawEnemyBullets model
        , drawExplosions model

        -- UI
        , drawScore model
        , drawLevel model
        , drawLifes model
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

        alpha_ =
            0.7
    in
        [ bg
            |> moveX x
            |> alpha alpha_
        , bg
            |> moveX (x - stageWidth)
            |> alpha alpha_
        ]


drawBgFar : Model -> List Form
drawBgFar model =
    drawBg model "assets/bg-clouds.png" 40


drawBgMedium : Model -> List Form
drawBgMedium model =
    drawBg model "assets/bg-hills.png" 10


drawBgNear : Model -> List Form
drawBgNear model =
    drawBg model "assets/bg-cars.png" 5


drawPowerUps : Model -> List Form
drawPowerUps model =
    case model.powerUp of
        Nothing ->
            []

        Just pu ->
            let
                ( file, w, h ) =
                    powerUpAsset

                form =
                    Element.image w h file
                        |> toForm
                        |> move pu.position
            in
                [ form ]


drawShip : Model -> List Form
drawShip model =
    let
        (Ship point) =
            model.playerShip

        file =
            "assets/ship.png"

        ( eFile, ew, eh ) =
            bigExplosion

        ( e2File, e2w, e2h ) =
            smallExplosion

        bigExplosionForm =
            Element.image ew eh eFile |> toForm

        smallExplosionForm =
            Element.image e2w e2h e2File |> toForm

        shipForm =
            Element.image shipWidth shipHeight file |> toForm

        --movingUpOrDown =
        --addStrech : Form -> Form
        --addStrech form =
        --    if model.pressedKeys.up then
        --        groupTransform (Transform.scaleY 0.8) [ form ]
        --    else
        --        form
        fire =
            ngon 3 6
                |> filled Color.yellow
                |> move ( -12, 0 )
                |> rotate (degrees 180)

        addTrail form =
            if model.pressedKeys.right && not model.pressedKeys.left then
                group [ form, fire ]
            else
                form

        form =
            if model.lifes == 0 then
                bigExplosionForm
            else if model.respawnIn > (respawnTime - 0.5 * Time.second) then
                smallExplosionForm
            else if model.respawnIn > 0 then
                shipForm |> alpha 0.5
            else
                shipForm |> addTrail
    in
        form
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
                |> move ( rightBoundary - 40, bottomBoundary - 20 )
    in
        [ form
        ]


drawLevel : Model -> List Form
drawLevel model =
    let
        level =
            model.level |> toString
    in
        "Level: "
            ++ level
            |> Text.fromString
            |> Text.color Color.darkGrey
            |> Collage.text
            |> move ( leftBoundary + 120, bottomBoundary - 20 )
            |> List.singleton


drawLifes : Model -> List Form
drawLifes model =
    let
        drawLife : Int -> Form
        drawLife pos =
            Text.fromString "❤️"
                |> Collage.text
                |> move ( leftBoundary + 20 + (toFloat pos * 16), bottomBoundary - 20 )
    in
        model.lifes
            |> List.range 1
            |> List.map drawLife


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
        if model.lifes == 0 then
            [ form ]
        else
            []


drawBullet : Bullet -> Form
drawBullet (Bullet point) =
    rect bulletWidth bulletHeight
        |> filled (Color.rgb 0 150 150)
        |> move point


drawBullets : Model -> List Form
drawBullets model =
    List.map drawBullet model.friendlyBullets


drawEnemyBullet : EnemyBullet -> Form
drawEnemyBullet bullet =
    rect enemyBulletWidth enemyBulletHeight
        |> filled (Color.rgb 255 100 0)
        |> move bullet.position


drawEnemyBullets : Model -> List Form
drawEnemyBullets model =
    List.map drawEnemyBullet model.enemyBullets
