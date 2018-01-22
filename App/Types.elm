module App.Types exposing (..)

import Window exposing (Size)
import Canvas exposing (Canvas)
import Time exposing (Time)

type Msg = MouseMove Int Int | WindowSize Window.Size | Tick Time

type alias Puck = { x: Float
    , y: Float
    , size: Float
    , xSpeed: Float
    , ySpeed: Float
}

type alias Striker = { x: Float
    , y: Float
    , size: Float
    , xSpeed: Float
    , ySpeed: Float }

type alias Model = { mouseX: Int
    , mouseY: Int
    , canvas: Canvas
    , puck: Puck
    , size: Canvas.Size
    , striker: Striker
}

type alias Circle = { radius: Float
    , x: Float
    , y: Float
}