module App.State exposing (..)

import App.Types exposing (..)
import App.Logic exposing (..)
import Canvas exposing (..)
import Window exposing (..)
import Task exposing (..)
import Color exposing (..)
import Mouse exposing (..)
import Time exposing (Time, second)

size: Canvas.Size
size = {
        width = 800
        , height = 800
    }

initPuck: Puck
initPuck = {x = 500, y = 300, size = 20, xSpeed = 0, ySpeed = 0}
initStriker: Striker
initStriker = {x = 0, y = 0, size = 20, xSpeed = 0, ySpeed = 0}
init: (Model, Cmd Msg)
init = 
    ({mouseX = 0
    , mouseY = 0
    , canvas = Canvas.initialize size
    , puck = initPuck
    , striker = initStriker
    , size = size }, Task.perform  WindowSize Window.size)

update: Msg -> Model -> (Model, Cmd Msg)

update msg model =
    case msg of
        WindowSize size -> 
            let
                canvasSize: Canvas.Size
                canvasSize = { width = size.width, height = size.height }
            in
                ({model | canvas = Canvas.initialize canvasSize
                   , size = canvasSize  }, Cmd.none)
        MouseMove x y ->
            let
                oldStriker = model.striker
                
            in
                ({ model | mouseX = x
                , mouseY = y }
                , Cmd.none)
        Tick _ ->
            let
                puck: Puck
                puck =  model.puck
                oldStriker = model.striker
                incrementedPuck = { puck | x = puck.x + puck.xSpeed
                    , y = puck.y + puck.ySpeed }
                newX = toFloat model.mouseX - oldStriker.size / 4
                newY = toFloat model.mouseY - oldStriker.size / 4
                strikerWithSpeed = { oldStriker | 
                     xSpeed = abs (newX - oldStriker.x)
                    , ySpeed = abs (newY - oldStriker.y)
                    , x = newX
                    , y = newY }
                batch = createBatch model
                newCanvas = Canvas.draw batch model.canvas
            in
                ({ model | puck = calcPuckStrike (wallBounce incrementedPuck model.size) strikerWithSpeed
                 , canvas = newCanvas
                 , striker = strikerWithSpeed }, Cmd.none)



createBatch : Model -> DrawOp
createBatch model = 
    let 
        puck: Canvas.DrawOp
        puck = createPuck model.puck       
        striker = createStriker model.striker
    in
        Canvas.batch
            [ ClearRect { x = 0, y = 0 } { width = model.size.width, height = model.size.height }
                , puck
                , striker
            ]

createPuck : Puck -> DrawOp
createPuck puck = 
    let
        fullDegrees = 2 * pi
    in
         Canvas.batch
            [ FillStyle <| Color.rgb
                200 0 0
            , BeginPath
            , Arc (Point puck.x puck.y) puck.size 0 fullDegrees
            , ClosePath
            , Fill
            ]   

createStriker : Striker -> DrawOp
createStriker striker = 
    let
        fullDegrees = 2 * pi
    in
         Canvas.batch
            [ FillStyle <| Color.rgb
                0 200 0
            , BeginPath
            , Arc (Point striker.x striker.y) striker.size 0 fullDegrees
            , ClosePath
            , Fill
            ]

subscriptions: Model -> Sub Msg
subscriptions model = 
    Sub.batch [
        Mouse.moves (\{x, y} -> MouseMove x y)
        ,   Time.every (second / 30) Tick
    ]