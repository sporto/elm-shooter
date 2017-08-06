module Bullet exposing (..)

import Constants exposing (..)
import Models exposing (..)
import Position
import Time exposing (Time)


new : Point -> Direction -> Float -> Bullet
new position direction speed =
    { position = position
    , direction = direction
    , speed = speed
    }


move : Time -> Bullet -> Bullet
move diff bullet =
    let
        movement =
            movementForDiff bullet.speed diff

        ( x, y ) =
            bullet.position

        position_ =
            case bullet.direction of
                DirectionLeft ->
                    ( x - movement, y )

                DirectionRight ->
                    ( x + movement, y )

                DirectionUp ->
                    ( x, y - movement )

                DirectionDown ->
                    ( x, y + movement )
    in
        { bullet | position = position_ }


isInStage : Bullet -> Bool
isInStage bullet =
    Position.isInStage bullet.position
