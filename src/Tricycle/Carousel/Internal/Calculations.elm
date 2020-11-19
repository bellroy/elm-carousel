module Tricycle.Carousel.Internal.Calculations exposing
    ( clonesAmountPerSide
    , distanceWithFrictionInLength
    , flickDirection
    , getLength
    , indexFromPosition
    , positionForIndex
    , saveIndex
    , slideLength
    , trackLength
    )

import Tricycle.Carousel.Internal.Direction as Direction exposing (Direction)
import Tricycle.Carousel.Internal.Distance as Distance exposing (Distance)
import Tricycle.Carousel.Internal.Indexes as Indexes exposing (Index, Track)
import Tricycle.Carousel.Internal.Position as Position exposing (Position)
import Tricycle.Carousel.Settings exposing (Settings)


getLength : Settings -> Float
getLength { horizontal, width, height } =
    if horizontal then
        width

    else
        height


positionForIndex : Settings -> Index a -> Position
positionForIndex settings index =
    clonesAmountPerSide settings
        |> (+) (Indexes.toInt index)
        |> negate
        |> toFloat
        |> (*) (slideLength settings)
        |> Position.fromFloat


indexFromPosition : Settings -> Position -> Index Track
indexFromPosition settings position =
    let
        p =
            Position.toFloat position

        sl =
            slideLength settings

        tl =
            trackLength settings

        cps =
            clonesAmountPerSide settings
    in
    (if not settings.infinite && negate p < 0 then
        0

     else if not settings.infinite && negate p > (tl - sl) then
        settings.amountOfSlides - 1

     else
        (round (p / sl) + cps)
            |> negate
    )
        |> Indexes.fromInt


clonesAmountPerSide : Settings -> Int
clonesAmountPerSide { slidesToShow, slidesToScroll, infinite } =
    if infinite then
        slidesToShow + slidesToScroll

    else
        0


slideLength : Settings -> Float
slideLength settings =
    settings.slidesToShow
        |> toFloat
        |> (/) (getLength settings)


trackLength : Settings -> Float
trackLength ({ amountOfSlides } as carouselRecord) =
    toFloat (amountOfSlides + clonesAmountPerSide carouselRecord)
        * slideLength carouselRecord


distanceWithFrictionInLength : (Float -> Float) -> Float -> Float -> Float
distanceWithFrictionInLength friction length distance =
    (1 / length)
        |> (*) distance
        |> abs
        |> friction
        |> (*) distance


flickDirection : Settings -> Distance -> Maybe Direction
flickDirection settings distance =
    let
        actualDistance =
            distanceWithFrictionInLength settings.friction (getLength settings) (Distance.toFloat distance)

        ratioTraveled =
            (1 / getLength settings) * actualDistance
    in
    if abs ratioTraveled > settings.threshold then
        negate ratioTraveled
            |> Direction.fromNumber

    else
        Nothing


saveIndex : Settings -> Index a -> Index Track
saveIndex settings suggestedIndex =
    if settings.infinite then
        saveInfiniteIndex settings suggestedIndex

    else
        case Indexes.compare suggestedIndex 0 of
            LT ->
                Indexes.fromInt 0

            _ ->
                case Indexes.compare suggestedIndex settings.amountOfSlides of
                    LT ->
                        Indexes.track suggestedIndex

                    _ ->
                        (settings.amountOfSlides - 1)
                            |> Indexes.fromInt


saveInfiniteIndex : Settings -> Index a -> Index Track
saveInfiniteIndex settings suggestedIndex =
    case Indexes.compare suggestedIndex (negate (clonesAmountPerSide settings)) of
        LT ->
            Indexes.fromInt 0

        _ ->
            case Indexes.compare suggestedIndex (settings.amountOfSlides + clonesAmountPerSide settings - settings.slidesToShow) of
                LT ->
                    Indexes.track suggestedIndex

                _ ->
                    Indexes.map (\a -> a - 1) suggestedIndex
                        |> Indexes.track
