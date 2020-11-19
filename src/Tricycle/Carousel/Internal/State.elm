module Tricycle.Carousel.Internal.State exposing (State, currentPositionIncludingDrag, init, update)

import Tricycle.Carousel.Internal.Animation as Animation
import Tricycle.Carousel.Internal.Calculations as Calc
import Tricycle.Carousel.Internal.Clock as Clock
import Tricycle.Carousel.Internal.Direction exposing (Direction(..))
import Tricycle.Carousel.Internal.Distance as Distance
import Tricycle.Carousel.Internal.Indexes as Indexes exposing (Active, Index, Track)
import Tricycle.Carousel.Internal.Movement exposing (Movement(..))
import Tricycle.Carousel.Internal.Msg exposing (Msg(..))
import Tricycle.Carousel.Internal.Position as Position exposing (Position)
import Tricycle.Carousel.Settings exposing (Settings)


type alias State =
    { activeIndex : Index Active
    , currentPosition : Position
    , movement : Movement
    , mouseOver : Bool
    }


init : Settings -> State
init settings =
    { currentPosition = Indexes.fromInt settings.startingIndex |> Calc.positionForIndex settings
    , activeIndex =
        Indexes.fromInt settings.startingIndex
            |> Indexes.fromIndex
    , movement = None
    , mouseOver = False
    }


update : Settings -> Msg -> State -> State
update settings msg state =
    case msg of
        Reset ->
            if Indexes.toInt state.activeIndex < 0 then
                (settings.amountOfSlides
                    - Indexes.toInt state.activeIndex
                    - 2
                )
                    |> Indexes.fromInt
                    |> WarpToIndex
                    |> updateF settings state

            else if Indexes.toInt state.activeIndex >= settings.amountOfSlides then
                (Indexes.toInt state.activeIndex
                    - settings.amountOfSlides
                )
                    |> Indexes.fromInt
                    |> WarpToIndex
                    |> updateF settings state

            else
                Indexes.fromActiveIndex state.activeIndex
                    |> WarpToIndex
                    |> updateF settings state

        IndexFromPosition ->
            let
                curPosIncl =
                    currentPositionIncludingDrag settings state.movement state.currentPosition
            in
            { state
                | activeIndex = Indexes.fromIndex <| Calc.indexFromPosition settings curPosIncl
            }

        WarpToIndex newIndex ->
            { state
                | activeIndex = Indexes.fromIndex newIndex
                , currentPosition = Calc.positionForIndex settings newIndex
                , movement = None
            }

        AnimateToIndex newIndex ->
            let
                newPosition =
                    Calc.positionForIndex settings newIndex

                curPosIncl =
                    currentPositionIncludingDrag settings state.movement state.currentPosition

                updatedState =
                    { state
                        | activeIndex = Indexes.fromIndex newIndex
                        , movement =
                            Animation.fromPositionToPosition settings curPosIncl newPosition
                                |> ByAnimation Clock.new
                    }
            in
            updatedState

        OnAnimationFrameDelta delta ->
            case state.movement of
                None ->
                    state

                ByDragging _ _ _ ->
                    state

                ByAnimation clock animation ->
                    let
                        updatedClock =
                            Clock.tick delta clock
                    in
                    if Animation.isDone updatedClock animation then
                        { state | movement = None }
                            |> update settings Reset

                    else
                        let
                            updatedPosition =
                                Animation.animate updatedClock animation
                                    |> Position.fromFloat

                            updatedMovement =
                                ByAnimation updatedClock animation

                            updatedState =
                                { state
                                    | currentPosition = updatedPosition
                                    , movement = updatedMovement
                                }
                        in
                        updatedState

        Previous ->
            applySlidesToScroll settings
                (Just Backwards)
                state.activeIndex
                |> AnimateToIndex
                |> updateF settings state

        Next ->
            applySlidesToScroll settings
                (Just Forwards)
                state.activeIndex
                |> AnimateToIndex
                |> updateF settings state

        DragStart startIndex startPosition ->
            let
                updatedMovement =
                    ByDragging startIndex startPosition Distance.none

                updatedState =
                    { state | movement = updatedMovement }
            in
            updatedState

        DragMove startIndex startPosition currentPosition ->
            let
                updatedMovement =
                    Distance.fromPositions startPosition currentPosition
                        |> ByDragging startIndex startPosition

                updatedState =
                    { state | movement = updatedMovement }
            in
            update settings IndexFromPosition updatedState

        DragEnd startIndex distance ->
            (if startIndex == state.activeIndex then
                applySlidesToScroll settings
                    (Calc.flickDirection settings distance)
                    state.activeIndex

             else
                Indexes.track state.activeIndex
            )
                |> AnimateToIndex
                |> updateF settings state

        OnKeyPress maybeDirection ->
            applySlidesToScroll settings maybeDirection state.activeIndex
                |> AnimateToIndex
                |> updateF settings { state | movement = None }

        OnScrollWheel distance ->
            let
                d =
                    Distance.toFloat distance
            in
            (if d > 0 then
                applySlidesToScroll settings (Just Forwards) state.activeIndex

             else if d < 0 then
                applySlidesToScroll settings (Just Backwards) state.activeIndex

             else
                Calc.saveIndex settings state.activeIndex
            )
                |> AnimateToIndex
                |> updateF settings { state | movement = None }

        OnAutoplayTick ->
            applySlidesToScroll settings (Just Forwards) state.activeIndex
                |> AnimateToIndex
                |> updateF settings { state | movement = None }

        OnMouseEnter ->
            { state | mouseOver = True }

        OnMouseLeave ->
            { state | mouseOver = False }


{-| flipper the flip
-}
updateF : Settings -> State -> Msg -> State
updateF settings state msg =
    update settings msg state


applySlidesToScroll : Settings -> Maybe Direction -> Index Active -> Index Track
applySlidesToScroll settings direction =
    (if direction == Just Forwards then
        Indexes.map (\a -> a + settings.slidesToScroll)

     else if direction == Just Backwards then
        Indexes.map (\a -> a - settings.slidesToScroll)

     else
        identity
    )
        >> Calc.saveIndex settings


currentPositionIncludingDrag : Settings -> Movement -> Position -> Position
currentPositionIncludingDrag settings movement currentPosition =
    case movement of
        None ->
            currentPosition

        ByDragging _ _ distance ->
            let
                distanceWithFrictionApplied =
                    Calc.distanceWithFrictionInLength
                        settings.friction
                        (Calc.getLength settings)
                        (Distance.toFloat distance)
            in
            (Position.toFloat currentPosition + distanceWithFrictionApplied)
                |> Position.fromFloat

        ByAnimation _ _ ->
            currentPosition
