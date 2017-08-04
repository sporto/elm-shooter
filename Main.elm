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


type alias Model =
    { coor : ( Float, Float )
    , currentKey : Maybe Key
    }


type Key
    = Up
    | Down
    | Left
    | Right
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
    collage 400
        400
        [ player model
        ]
        |> Element.toHtml


player : Model -> Form
player model =
    rect 20 20
        |> filled (Color.rgb 0 0 0)
        |> move model.coor


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
            in
                ( { model | coor = newCoor }, Cmd.none )


main =
    Html.program
        { update = update
        , view = view
        , subscriptions = subscriptions
        , init = init
        }
