port module Audio exposing (..)


playExplosion : Cmd msg
playExplosion =
    playSound "explosion.mp3"


playExplosionBig : Cmd msg
playExplosionBig =
    playSound "flashbang.mp3"


playLaser : Cmd msg
playLaser =
    playSound "laser.mp3"


port playSound : String -> Cmd msg
