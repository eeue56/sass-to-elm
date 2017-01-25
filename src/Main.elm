module Main exposing (..)

import Home exposing (model, update, view)
import Json.Decode as Json
import Html


main =
    Html.program { init = ( model, Cmd.none ), view = view, update = update, subscriptions = (\_ -> Sub.none) }
