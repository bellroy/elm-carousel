module Tricycle.Carousel.Internal.Movement exposing (Movement(..))

import Tricycle.Carousel.Internal.Animation exposing (Animation)
import Tricycle.Carousel.Internal.Clock exposing (Clock)
import Tricycle.Carousel.Internal.Distance exposing (Distance)
import Tricycle.Carousel.Internal.Indexes exposing (Active, Index)
import Tricycle.Carousel.Internal.Position exposing (Position)


type Movement
    = None
    | ByDragging (Index Active) Position Distance
    | ByAnimation Clock Animation
