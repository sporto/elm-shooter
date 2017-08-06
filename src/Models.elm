module Models exposing (..)

import Time exposing (Time)
import Keyboard
import Constants exposing (..)


-- MODELS


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
