module Tricycle.Carousel.Internal.Position exposing
    ( Position
    , decodePagePosition
    , fromFloat
    , toFloat
    )

import Json.Decode as Decode exposing (Decoder)
import Tricycle.Carousel.Settings as Settings exposing (Settings)


type Position
    = Position Float


fromFloat : Float -> Position
fromFloat =
    Position


toFloat : Position -> Float
toFloat (Position float) =
    float


decodePagePosition : Settings -> Decoder Position
decodePagePosition settings =
    let
        decodePagePositionField =
            let
                field =
                    if settings.horizontal then
                        "pageX"

                    else
                        "pageY"
            in
            Decode.map Position
                (Decode.field field Decode.float)
    in
    Decode.oneOf
        [ decodePagePositionField
        , Decode.at [ "touches", "0" ] decodePagePositionField
        ]
