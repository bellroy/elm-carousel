module Tricycle.Carousel.Internal.Direction exposing (Direction(..), decodeFromKey, fromNumber)

import Json.Decode as Decode exposing (Decoder)
import Tricycle.Carousel.Settings exposing (Settings)


type Direction
    = Backwards
    | Forwards


fromNumber : number -> Maybe Direction
fromNumber a =
    if a < 0 then
        Just Backwards

    else if a > 0 then
        Just Forwards

    else
        Nothing


decodeFromKey : Settings -> Decoder (Maybe Direction)
decodeFromKey settings =
    Decode.field "key" Decode.string
        |> Decode.map
            (\key ->
                if settings.horizontal then
                    case key of
                        "ArrowRight" ->
                            Just Forwards

                        "ArrowLeft" ->
                            Just Backwards

                        "Right" ->
                            Just Forwards

                        "Left" ->
                            Just Backwards

                        _ ->
                            Nothing

                else
                    case key of
                        "ArrowDown" ->
                            Just Forwards

                        "ArrowUp" ->
                            Just Backwards

                        "Down" ->
                            Just Forwards

                        "Up" ->
                            Just Backwards

                        _ ->
                            Nothing
            )
