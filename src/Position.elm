module Position exposing (..)

import Models exposing (..)


moveY : Float -> Point -> Point
moveY dist ( x, y ) =
    ( x, y + dist )
