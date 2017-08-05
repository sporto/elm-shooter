module Models exposing (..)

import Time exposing (Time)
import Keyboard


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
    0.3 * Time.second


enemyWeaponCooldownTime =
    5 * Time.second


shipMovementForDiff diff =
    diff / 2


bulletMovementForDiff diff =
    diff


enemyBulletMovementForDiff diff =
    diff / 6


weaponCooldownForDiff diff =
    diff


bgMovementForDiff : Float -> Float -> Float
bgMovementForDiff time distance =
    time * -1 / distance


shipWidth =
    20


shipHeight =
    20


enemyWidth =
    12


enemyHeight =
    12


bulletWidth =
    10


bulletHeight =
    4


enemyBulletHeight =
    4


enemyBulletWidth =
    6


explosionWidth =
    20


explosionHeight =
    20


bigExplosion =
    ( "assets/explosion-big.png", 32, 32 )


smallExplosion =
    ( "assets/explosion.png", explosionWidth, explosionHeight )


respawnTime =
    2 * Time.second



-- MODELS


type Direction
    = DirectionLeft
    | DirectionRight


type alias Point =
    ( Float, Float )


type Bullet
    = Bullet Point


type alias EnemyBullet =
    { position : Point
    , direction : Direction
    }


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
    , weaponCooldown : Float
    }


newEnemy : Time -> Enemy
newEnemy time =
    { createdTime = time
    , direction = DirectionLeft
    , position = ( rightBoundary, 0 )
    , weaponCooldown = 1 * Time.second
    }


type alias Model =
    { enemies : List Enemy
    , enemyBullets : List EnemyBullet
    , explosions : List Explosion
    , friendlyBullets : List Bullet
    , lifes : Int
    , playerShip : Ship
    , pressedKeys :
        { up : Bool
        , down : Bool
        , left : Bool
        , right : Bool
        }
    , respawnIn : Time
    , score : Int
    , time : Float
    , weaponCooldown : Float
    }


initialModel : Model
initialModel =
    { enemies = []
    , enemyBullets = []
    , explosions = []
    , friendlyBullets = []
    , lifes = 3
    , playerShip = Ship ( 0, 0 )
    , pressedKeys =
        { up = False
        , down = False
        , left = False
        , right = False
        }
    , respawnIn = 0
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


type alias Return a =
    ( Model, Cmd a )
