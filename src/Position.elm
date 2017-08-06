module Position exposing (..)

import Constants exposing (..)
import Models exposing (..)


moveY : Float -> Point -> Point
moveY dist ( x, y ) =
    ( x, y + dist )


isInStage : Point -> Bool
isInStage ( x, y ) =
    (x >= leftBoundary) && (x <= rightBoundary)
