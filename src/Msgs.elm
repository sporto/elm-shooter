module Msgs exposing (..)

import Time exposing (Time)
import Keyboard
import Models exposing (..)


type Msg
    = NoOp
    | OnKeyDown Keyboard.KeyCode
    | OnKeyUp Keyboard.KeyCode
    | OnAnimationFrame Time
