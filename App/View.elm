module App.View exposing (view)

import App.Types exposing (..)
import Html exposing (..)
import Canvas exposing (Canvas)

view: Model -> Html Msg
view model = 
    div []
        [ Canvas.toHtml []
            model.canvas
        ]