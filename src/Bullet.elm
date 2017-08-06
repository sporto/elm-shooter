module Bullet exposing (..)

import Models exposing (..)
import Time exposing (Time)


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
    let
        ( x, y ) =
            bullet.position
    in
        (x > leftBoundary)
            && (x < rightBoundary)
