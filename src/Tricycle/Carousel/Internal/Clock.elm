module Tricycle.Carousel.Internal.Clock exposing (Clock, new, tick, toFloat)


type Clock
    = Clock Float


new : Clock
new =
    Clock 0


tick : Float -> Clock -> Clock
tick delta =
    toFloat >> (+) delta >> Clock


toFloat : Clock -> Float
toFloat (Clock float) =
    float
