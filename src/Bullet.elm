module Bullet exposing (..)

import Models exposing (..)


move : Float -> Bullet -> Bullet
move movement bullet =
    let
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
