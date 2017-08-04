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
    , currentKey : Maybe Key
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



-- INIT


initialModel : Model
initialModel =
    { coor = ( 0, 0 )
    , currentKey = Nothing
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


rightBoundary =
    800


view : Model -> Html msg
view model =
    collage rightBoundary
        400
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
            let
                key =
                    keyCodeToKey keyCode
            in
                case key of
                    Space ->
                        ( tryShootBullet model, Cmd.none )

                    OtherKey ->
                        ( { model | currentKey = Nothing }, Cmd.none )

                    _ ->
                        ( { model | currentKey = Just key }, Cmd.none )

        OnKeyUp _ ->
            ( { model | currentKey = Nothing }, Cmd.none )

        OnAnimationFrame time ->
            let
                ( currentX, currentY ) =
                    model.coor

                newCoor =
                    case model.currentKey of
                        Just Up ->
                            ( currentX, currentY + movement )

                        Just Down ->
                            ( currentX, currentY - movement )

                        Just Left ->
                            ( currentX - movement, currentY )

                        Just Right ->
                            ( currentX + movement, currentY )

                        _ ->
                            model.coor

                newBullet =
                    getNewBulletPosition model.bullet
            in
                ( { model | coor = newCoor, bullet = newBullet }, Cmd.none )


tryShootBullet : Model -> Model
tryShootBullet model =
    case model.bullet of
        Nothing ->
            { model | bullet = Just model.coor }

        _ ->
            model


getNewBulletPosition bullet =
    case bullet of
        Just ( x, y ) ->
            if x > rightBoundary then
                Nothing
            else
                Just ( x + (movement * 3), y )

        Nothing ->
            Nothing


main =
    Html.program
        { update = update
        , view = view
        , subscriptions = subscriptions
        , init = init
        }
