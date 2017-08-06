module Constants exposing (..)

import Time exposing (Time)


type alias Asset =
    ( String, Int, Int )


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
    Time.second * 45


initialPowerUpDelay : Time
initialPowerUpDelay =
    10 * Time.second


maxLevel : Int
maxLevel =
    3
