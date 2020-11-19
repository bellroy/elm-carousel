module Tricycle.Carousel.Internal.Distance exposing (Distance, fromPositions, none, toFloat)

import Tricycle.Carousel.Internal.Position as Position exposing (Position)


type Distance
    = Distance Float


none : Distance
none =
    Distance 0


fromPositions : Position -> Position -> Distance
fromPositions a b =
    (Position.toFloat b - Position.toFloat a)
        |> Distance


toFloat : Distance -> Float
toFloat (Distance float) =
    float
