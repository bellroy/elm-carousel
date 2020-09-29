module Main exposing (main)

import Browser exposing (element)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Tricycle.Carousel exposing (..)

main : Program () Carousel Msg
main =
    element
        { init = \_ -> (init, Cmd.none)
        , update = (\a b -> (update a b, Cmd.none))
        , subscriptions = subscriptions
        , view = view
        }

 