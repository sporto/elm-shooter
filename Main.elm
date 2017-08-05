module Main exposing (..)

import AnimationFrame
import Debug
import GameLoop
import Html exposing (..)
import Html.Attributes exposing (width, height, style)
import Keyboard
import Models exposing (..)
import Msgs exposing (..)
import Time exposing (Time)
import UserEvents
import Views


-- INIT


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.lives == 0 then
        Sub.none
    else
        Sub.batch
            [ AnimationFrame.diffs OnAnimationFrame
            , Keyboard.downs OnKeyDown
            , Keyboard.ups OnKeyUp
            ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnKeyDown keyCode ->
            UserEvents.handleKeyDown model keyCode

        OnKeyUp keyCode ->
            UserEvents.handleKeyUp model keyCode

        OnAnimationFrame time ->
            GameLoop.updateAnimationFrame model time



-- MAIN


main =
    Html.program
        { update = update
        , view = Views.view
        , subscriptions = subscriptions
        , init = init
        }
