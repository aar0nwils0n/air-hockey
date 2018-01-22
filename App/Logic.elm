module App.Logic exposing (..)

import App.Types exposing (..)
import Canvas exposing (Canvas)

strikePuck : Puck -> Striker -> Puck
strikePuck puck striker = 
    let
        angle = atan2 ( striker.y - puck.y ) ( striker.x - puck.x ) 
    in
        { puck | xSpeed = (( cos angle ) * -(striker.xSpeed + abs puck.xSpeed)) -
            (( cos angle ) * (abs puck.ySpeed))
        , ySpeed = (( sin angle ) * -(striker.ySpeed + abs puck.ySpeed)) -
            (( sin angle ) * (abs puck.xSpeed)) }

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


circlesCollide : Circle -> Circle -> Bool
circlesCollide c1 c2 =
    let
        xDiff = abs c1.x - c2.x
        yDiff = abs c1.y - c2.y
        hypot =  sqrt (xDiff^2 + yDiff^2)
    in
        (hypot - c1.radius - c2.radius) <= 0