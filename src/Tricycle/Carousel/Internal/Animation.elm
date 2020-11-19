module Tricycle.Carousel.Internal.Animation exposing (Animation, animate, fromPositionToPosition, isDone)

import Animation as AnimationInterface
import Tricycle.Carousel.Internal.Calculations as Calc
import Tricycle.Carousel.Internal.Clock as Clock exposing (Clock)
import Tricycle.Carousel.Internal.Distance as Distance
import Tricycle.Carousel.Internal.Position as Position exposing (Position)
import Tricycle.Carousel.Settings exposing (Settings)


type alias Animation =
    AnimationInterface.Animation


fromPositionToPosition : Settings -> Position -> Position -> Animation
fromPositionToPosition settings a b =
    let
        transitionSpeedInMsPerPx =
            toFloat settings.transitionSpeedInMs / Calc.slideLength settings

        transitionTimeRemaining =
            Distance.fromPositions a b
                |> Distance.toFloat
                |> abs
                |> (*) transitionSpeedInMsPerPx
                |> (\r -> min r (toFloat settings.transitionSpeedInMs))
    in
    AnimationInterface.animation 0
        |> AnimationInterface.from (Position.toFloat a)
        |> AnimationInterface.to (Position.toFloat b)
        |> AnimationInterface.ease settings.easing
        |> AnimationInterface.duration transitionTimeRemaining


isDone : Clock -> Animation -> Bool
isDone clock =
    AnimationInterface.isDone (Clock.toFloat clock)


animate : Clock -> Animation -> Float
animate clock =
    AnimationInterface.animate (Clock.toFloat clock)
