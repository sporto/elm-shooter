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


weaponCooldownTime : Time
weaponCooldownTime =
    0.3 * Time.second


enemyWeaponCooldownTime : Time
enemyWeaponCooldownTime =
    5 * Time.second


shipMovementForDiff : Time -> Float
shipMovementForDiff diff =
    diff / 2



-- 1 is Ship speed


movementForDiff : Float -> Time -> Float
movementForDiff speed diff =
    diff * speed / 4


friendlyBulletSpeed : Float
friendlyBulletSpeed =
    3


enemyBulletSpeed : Float
enemyBulletSpeed =
    0.5


weaponCooldownForDiff : Time -> Float
weaponCooldownForDiff diff =
    diff


powerUpMovementForDiff : Time -> Float
powerUpMovementForDiff diff =
    diff / 25


bgMovementForDiff : Float -> Float -> Float
bgMovementForDiff time distance =
    time * -1 / distance


shipWidth : Int
shipWidth =
    20


shipHeight : Int
shipHeight =
    20


enemyWidth : Int
enemyWidth =
    12


enemyHeight : Int
enemyHeight =
    12


bulletWidth : Int
bulletWidth =
    10


bulletHeight : Int
bulletHeight =
    4


enemyBulletHeight : Int
enemyBulletHeight =
    4


enemyBulletWidth : Int
enemyBulletWidth =
    6


explosionWidth : Int
explosionWidth =
    20


explosionHeight : Int
explosionHeight =
    20


bigExplosion : Asset
bigExplosion =
    ( "assets/explosion-big.png", 32, 32 )


smallExplosion : Asset
smallExplosion =
    ( "assets/explosion.png", explosionWidth, explosionHeight )


powerUpAsset : Asset
powerUpAsset =
    ( "assets/power-up.png", 24, 24 )


respawnTime : Float
respawnTime =
    2 * Time.second


powerUpFrequency : Float
powerUpFrequency =
    Time.second * 10



-- MODELS


type alias Asset =
    ( String, Int, Int )


type Direction
    = DirectionLeft
    | DirectionRight
    | DirectionUp
    | DirectionDown


type alias Point =
    ( Float, Float )


type alias Bullet =
    { position : Point
    , direction : Direction
    , speed : Float
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


type alias PowerUp =
    { position : Point
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
    , enemyBullets : List Bullet
    , explosions : List Explosion
    , friendlyBullets : List Bullet
    , lifes : Int
    , level : Int
    , playerShip : Ship
    , pressedKeys :
        { up : Bool
        , down : Bool
        , left : Bool
        , right : Bool
        }
    , powerUp : Maybe PowerUp
    , powerUpCount : Int
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
    , level = 0
    , lifes = 3
    , playerShip = Ship ( 0, 0 )
    , pressedKeys =
        { up = False
        , down = False
        , left = False
        , right = False
        }
    , powerUp = Nothing
    , powerUpCount = 0
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
