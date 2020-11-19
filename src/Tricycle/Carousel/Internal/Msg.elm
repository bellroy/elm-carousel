module Tricycle.Carousel.Internal.Msg exposing (Msg(..))

import Tricycle.Carousel.Internal.Direction exposing (Direction)
import Tricycle.Carousel.Internal.Distance exposing (Distance)
import Tricycle.Carousel.Internal.Indexes exposing (Active, Index, Track)
import Tricycle.Carousel.Internal.Position exposing (Position)


type Msg
    = Reset
    | IndexFromPosition
    | WarpToIndex (Index Track)
    | AnimateToIndex (Index Track)
    | OnAnimationFrameDelta Float
      --
    | Previous
    | Next
      --
    | DragStart (Index Active) Position
    | DragMove (Index Active) Position Position
    | DragEnd (Index Active) Distance
      --
    | OnKeyPress (Maybe Direction)
      --
    | OnScrollWheel Distance
      --
    | OnAutoplayTick
    | OnMouseEnter
    | OnMouseLeave
