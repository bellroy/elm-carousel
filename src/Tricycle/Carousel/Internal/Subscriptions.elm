module Tricycle.Carousel.Internal.Subscriptions exposing (subscriptions)

import Browser.Events as BrowserEvents
import Json.Decode as Decode
import Time
import Tricycle.Carousel.Internal.Direction as Direction
import Tricycle.Carousel.Internal.Movement exposing (Movement(..))
import Tricycle.Carousel.Internal.Msg exposing (Msg(..))
import Tricycle.Carousel.Internal.State exposing (State)
import Tricycle.Carousel.Settings exposing (Settings)


subscriptions : Settings -> State -> Sub Msg
subscriptions settings state =
    List.filterMap identity
        [ subscribeToKeyPresses settings
        , subscribeToAnimationFrameDelta state
        , subscribeToAutoplayTick settings state
        ]
        |> Sub.batch


subscribeToKeyPresses : Settings -> Maybe (Sub Msg)
subscribeToKeyPresses settings =
    if settings.keyboard then
        Direction.decodeFromKey settings
            |> Decode.map OnKeyPress
            |> BrowserEvents.onKeyUp
            |> Just

    else
        Nothing


subscribeToAnimationFrameDelta : State -> Maybe (Sub Msg)
subscribeToAnimationFrameDelta { movement } =
    case movement of
        None ->
            Nothing

        ByDragging _ _ _ ->
            Nothing

        ByAnimation _ _ ->
            BrowserEvents.onAnimationFrameDelta OnAnimationFrameDelta
                |> Just


subscribeToAutoplayTick : Settings -> State -> Maybe (Sub Msg)
subscribeToAutoplayTick settings state =
    if settings.autoplay && (not settings.pauseOnHover || not state.mouseOver) then
        Time.every (toFloat settings.autoplayIntervalInMs) (always OnAutoplayTick)
            |> Just

    else
        Nothing
