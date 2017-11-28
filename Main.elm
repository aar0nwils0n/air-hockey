import Html exposing (..)
import Canvas exposing (..)
import Mouse exposing (..)
import Color exposing (..)
import Window exposing (..)
import Task exposing (..)
import Time exposing (Time, second)

-- import Action exposing (Msg(WindowResize, NoOp), update)

-- import Html.Events exposing (..)


main =
    Html.program 
    {
        init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

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
                batch = createBatch model
                newCanvas = Canvas.draw batch model.canvas
                oldStriker = model.striker
                newX = toFloat x - oldStriker.size / 4
                newY = toFloat y - oldStriker.size / 4
                striker = { oldStriker | x = newX
                , y = newY
                , xSpeed = abs (oldStriker.xSpeed - toFloat x)
                , ySpeed = abs (oldStriker.ySpeed - toFloat y) }
            in
                ({ model | mouseX = x
                , mouseY = y
                , striker = striker
                , canvas = newCanvas }
                , Cmd.none)
        Tick _ ->
            let
                puck: Puck
                puck = 
                if 
                    circlesCollide { x =  model.puck.x
                        , y = model.puck.y
                        , radius = model.puck.size
                    }
                    { x = model.striker.x
                        , y = model.striker.y
                        , radius = model.striker.size
                    }
                then
                    strikePuck model.puck model.striker
                else
                    model.puck

                incrementedPuck =  { puck | x = puck.x + puck.xSpeed
                    , y = puck.y + puck.ySpeed}
            in
                ({ model | puck = incrementedPuck }, Cmd.none)

subscriptions: Model -> Sub Msg
subscriptions model = 
    Sub.batch [
        Mouse.moves (\{x, y} -> MouseMove x y)
        ,   Time.every (second / 24) Tick
    ]


type alias Circle = { radius: Float
    , x: Float
    , y: Float
}


circlesCollide : Circle -> Circle -> Bool
circlesCollide c1 c2 =
    let
        xDiff = abs c1.x - c2.x
        yDiff = abs c1.y - c2.y
        hypot =  sqrt (xDiff^2 + yDiff^2)
    in
        (hypot - c1.radius - c2.radius) <= 0



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

strikePuck : Puck -> Striker -> Puck
strikePuck puck striker = 
    let
        angle = atan2 ( striker.y - puck.y ) ( striker.x - puck.x ) 
    in
        { puck | xSpeed = ( cos angle ) * -striker.xSpeed * 0.025
        , ySpeed = ( sin angle ) * -striker.ySpeed * 0.025 }
        

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



distance : Model -> Float
distance model =
    let
        xDiff = abs model.puck.x - model.striker.x
        yDiff = abs model.puck.y - model.striker.y
        hypot =  sqrt (xDiff^2 + yDiff^2)
    in
        (hypot - model.puck.size - model.striker.size )



view: Model -> Html Msg
view model = 
    div []
        [ Canvas.toHtml []
            model.canvas
            , text <| toString <| distance model
        ]