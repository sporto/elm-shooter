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
    , weaponCooldown : Float
    , bullets : List Bullet
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



--baseMovement =
--    5
--bulletMovement =
--    baseMovement * 3


weaponCooldownTime =
    150


shipMovementForDiff diff =
    diff / 2


bulletMovementForDiff diff =
    diff


weaponCooldownForDiff diff =
    diff



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
    , bullets = []
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
    --let
    --    _ =
    --        Debug.log "bullets" model.bullets
    --in
    collage (truncate stageWidth)
        (truncate stageHeight)
        (drawActors model)
        |> Element.toHtml


drawActors : Model -> List Form
drawActors model =
    playerView model
        :: bulletsView model


playerView : Model -> Form
playerView model =
    rect 20 20
        |> filled (Color.rgb 0 0 0)
        |> move model.coor


bulletView : Bullet -> Form
bulletView bullet =
    rect 10 4
        |> filled (Color.rgb 0 0 0)
        |> move bullet


bulletsView : Model -> List Form
bulletsView model =
    List.map bulletView model.bullets


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
            updateAnimationFrame model time


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
            let
                bullets_ =
                    model.coor :: model.bullets
            in
                ( { model | bullets = bullets_, weaponCooldown = weaponCooldownTime }, msg )
        else
            ( model, msg )
    else
        ( model, msg )



-- CURRENT FRAME


updateAnimationFrame : Model -> Time.Time -> ( Model, Cmd msg )
updateAnimationFrame model diff =
    ( model, Cmd.none )
        |> updateShipMovement diff
        |> updateBulletsMovement diff
        |> updateWeaponCooldown diff


updateShipMovement : Time.Time -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateShipMovement diff ( model, msg ) =
    let
        movement =
            shipMovementForDiff diff

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


updateBulletsMovement : Time.Time -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateBulletsMovement diff ( model, msg ) =
    let
        movement =
            bulletMovementForDiff diff

        moveBullet ( x, y ) =
            ( x + movement, y )

        movedBullets =
            model.bullets
                |> List.map moveBullet
                |> List.filter (\( x, y ) -> x < rightBoundary)
    in
        ( { model | bullets = movedBullets }, msg )


updateWeaponCooldown : Time.Time -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateWeaponCooldown diff ( model, msg ) =
    let
        weaponCooldown_ =
            model.weaponCooldown
                |> (\current -> current - weaponCooldownForDiff diff)
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
