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

subscriptions: Model -> Sub Msg
subscriptions model = 
    Sub.batch [
        Mouse.moves (\{x, y} -> MouseMove x y)
        ,   Time.every (second / 30) Tick
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
        { puck | xSpeed = ( cos angle ) * -striker.xSpeed
        , ySpeed = ( sin angle ) * -striker.ySpeed }
        

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

incrementStrikerPosition : Striker -> Striker
incrementStrikerPosition striker = { striker | x = striker.x + striker.xSpeed
    , y = striker.y + striker.ySpeed }

calcPuckStrike : Puck -> Striker -> Puck
calcPuckStrike puck striker = 
    if 
        circlesCollide { x = puck.x
            , y = puck.y
            , radius = puck.size
        }
        { x = striker.x
            , y = striker.y
            , radius = striker.size
        }
    then
        strikePuck puck striker
    else
        puck

wallBounce : Puck -> Canvas.Size -> Puck
wallBounce puck canvasSize = 
    let
        xSpeed = 
            if puck.x <= puck.size || puck.x > toFloat canvasSize.width - puck.size
            then -puck.xSpeed
            else puck.xSpeed

        ySpeed = 
            if puck.y <= puck.size || puck.y > toFloat canvasSize.height - puck.size
            then -puck.ySpeed
            else puck.ySpeed
    in
        { puck | ySpeed = ySpeed, xSpeed = xSpeed }



view: Model -> Html Msg
view model = 
    div []
        [ Canvas.toHtml []
            model.canvas
            , text <| toString <| distance model
        ]