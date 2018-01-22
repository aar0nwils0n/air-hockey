import Html exposing (..)
import Canvas exposing (..)
import Mouse exposing (..)
import Color exposing (..)
import Window exposing (..)
import Task exposing (..)
import Time exposing (Time, second)
import App.State exposing (init, update, subscriptions)
import App.View exposing (view)

main =
    Html.program 
    {
        init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }












