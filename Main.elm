module Main exposing (..)

import AnimationFrame
import Collage exposing (..)
import Color
import Debug
import Element
import Html exposing (..)
import Html.Attributes exposing (width, height, style)
import Keyboard
import Time


-- MODELS


type alias Point =
    ( Float, Float )


type alias Bullet =
    Point


type alias Model =
    { coor : ( Float, Float )
    , pressedKeys :
        { up : Bool
        , down : Bool
        , left : Bool
        , right : Bool
        }
    , weaponCooldown : Int
    , bullet : Maybe Bullet
    }


type Key
    = Up
    | Down
    | Left
    | Right
    | Space
    | OtherKey



-- MSGs


type Msg
    = NoOp
    | OnKeyDown Keyboard.KeyCode
    | OnKeyUp Keyboard.KeyCode
    | OnAnimationFrame Time.Time



-- CONSTANTS


stageHeight : Float
stageHeight =
    400


stageWidth : Float
stageWidth =
    800


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



-- INIT


initialModel : Model
initialModel =
    { coor = ( 0, 0 )
    , pressedKeys =
        { up = False
        , down = False
        , left = False
        , right = False
        }
    , weaponCooldown = 0
    , bullet = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs OnAnimationFrame
        , Keyboard.downs OnKeyDown
        , Keyboard.ups OnKeyUp
        ]


view : Model -> Html msg
view model =
    collage (truncate stageWidth)
        (truncate stageHeight)
        [ playerView model
        , bulletView model
        ]
        |> Element.toHtml


playerView : Model -> Form
playerView model =
    rect 20 20
        |> filled (Color.rgb 0 0 0)
        |> move model.coor


bulletView : Model -> Form
bulletView model =
    case model.bullet of
        Just coors ->
            rect 10 4
                |> filled (Color.rgb 0 0 0)
                |> move coors

        Nothing ->
            group []


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


movement =
    5


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnKeyDown keyCode ->
            handleKeyDown model keyCode

        OnKeyUp keyCode ->
            handleKeyUp model keyCode

        OnAnimationFrame time ->
            handleAnimationFrame model time


handleKeyDown : Model -> Keyboard.KeyCode -> ( Model, Cmd msg )
handleKeyDown model keyCode =
    let
        key =
            keyCodeToKey keyCode

        currentKeys =
            model.pressedKeys

        updatedKeys =
            case key of
                Up ->
                    { currentKeys | up = True }

                Down ->
                    { currentKeys | down = True }

                Left ->
                    { currentKeys | left = True }

                Right ->
                    { currentKeys | right = True }

                _ ->
                    currentKeys

        updatedModel =
            { model | pressedKeys = updatedKeys }
    in
        ( updatedModel, Cmd.none )
            |> tryShootBullet key


handleKeyUp : Model -> Keyboard.KeyCode -> ( Model, Cmd Msg )
handleKeyUp model keyCode =
    let
        key =
            keyCodeToKey keyCode

        currentKeys =
            model.pressedKeys

        updatedKeys =
            case key of
                Up ->
                    { currentKeys | up = False }

                Down ->
                    { currentKeys | down = False }

                Left ->
                    { currentKeys | left = False }

                Right ->
                    { currentKeys | right = False }

                _ ->
                    currentKeys

        updatedModel =
            { model | pressedKeys = updatedKeys }
    in
        ( updatedModel, Cmd.none )


tryShootBullet : Key -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
tryShootBullet key ( model, msg ) =
    if key == Space then
        if model.weaponCooldown == 0 then
            ( { model | bullet = Just model.coor, weaponCooldown = 25 }, msg )
        else
            ( model, msg )
    else
        ( model, msg )



-- CURRENT FRAME


handleAnimationFrame : Model -> Time.Time -> ( Model, Cmd msg )
handleAnimationFrame model time =
    ( model, Cmd.none )
        |> handleShipMovement time
        |> handleBulletsMovement time
        |> handleWeaponCooldown time


handleShipMovement : Time.Time -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
handleShipMovement time ( model, msg ) =
    let
        plusUp ( x, y ) =
            if model.pressedKeys.up then
                ( x, y + movement )
            else
                ( x, y )

        plusDown ( x, y ) =
            if model.pressedKeys.down then
                ( x, y - movement )
            else
                ( x, y )

        plusLeft ( x, y ) =
            if model.pressedKeys.left then
                ( x - movement, y )
            else
                ( x, y )

        plusRight ( x, y ) =
            if model.pressedKeys.right then
                ( x + movement, y )
            else
                ( x, y )

        bound ( x, y ) =
            ( x
                |> min (rightBoundary - 10)
                |> max (leftBoundary + 10)
            , y
                |> min (bottomBoundary - 10)
                |> max (topBoundary + 10)
            )

        newCoor =
            model.coor
                |> plusUp
                |> plusDown
                |> plusLeft
                |> plusRight
                |> bound
    in
        ( { model | coor = newCoor }, msg )


handleBulletsMovement : Time.Time -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
handleBulletsMovement time ( model, msg ) =
    let
        newBullet =
            getNewBulletPosition model.bullet
    in
        ( { model | bullet = newBullet }, msg )


getNewBulletPosition bullet =
    case bullet of
        Just ( x, y ) ->
            if x > rightBoundary then
                Nothing
            else
                Just ( x + (movement * 3), y )

        Nothing ->
            Nothing


handleWeaponCooldown : Time.Time -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
handleWeaponCooldown time ( model, msg ) =
    let
        weaponCooldown_ =
            model.weaponCooldown
                - 1
                |> max 0
    in
        ( { model | weaponCooldown = weaponCooldown_ }, msg )



-- MAIN


main =
    Html.program
        { update = update
        , view = view
        , subscriptions = subscriptions
        , init = init
        }
