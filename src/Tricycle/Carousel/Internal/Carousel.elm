module Tricycle.Carousel.Internal.Carousel exposing (Carousel, init, updateSettings, updateState)

import Tricycle.Carousel.Internal.State as State exposing (State)
import Tricycle.Carousel.Settings exposing (Settings)


type Carousel
    = Carousel Settings State


init : Settings -> Carousel
init settings =
    State.init settings
        |> Carousel settings


updateSettings : Settings -> Carousel -> Carousel
updateSettings settings (Carousel _ state) =
    Carousel settings state


updateState : State -> Carousel -> Carousel
updateState state (Carousel settings _) =
    Carousel settings state
