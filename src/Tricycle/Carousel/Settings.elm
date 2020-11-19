module Tricycle.Carousel.Settings exposing
    ( Settings, default
    , updateAmountOfSlides, updateWidthAndHeight
    )

{-|

@docs Settings, default


## Helpers

@docs updateAmountOfSlides, updateWidthAndHeight

-}


{-| Configure your Carousel through the following settings.

  - **amountOfSlides** The amount of slides your carousel contains.
  - **initialIndex** The starting index of your carousel.
  - **slidesToShow** How many slides should the carousel display at a time.
  - **slidesToScroll** How many slides should the carousel scroll through on a keyboard, scrollWheel or "flick" event.
  - **width** The width of your carousel in px.
  - **height** The height of your carousel in px.
  - **horizontal** Should your carousel render in a horizontal (True) structure or vertical (False)
  - **infinite** Should your carousel mimic a infinite scrolling behaviour. E.g. after the last slide the first slide appears again and vice versa.
  - **transitionSpeedInMs** The time in ms it takes for a slide to scroll from one side to the other.
  - **easing** Use your own easing function to ease the transition.
    Check out [elm-community/easing-functions](https://package.elm-lang.org/packages/elm-community/easing-functions/latest/Ease) for some drop in easing functions.
  - **friction** Friction applied while dragging.
    Check out [elm-community/easing-functions](https://package.elm-lang.org/packages/elm-community/easing-functions/latest/Ease) for some drop in easing functions.
  - **treshold** The treshold for a drag movement to trigger a slide navigation, expressed as a ratio from 0 to 1 of the carousel's width or height
  - **keyboard** Enable arrow keys navigation.
  - **scrollWheel** Enable scroll wheel navigation.
  - **autoplay** Enable autoplay
  - **autoplayIntervalInMs** Autoplay interval in ms when enabled
  - **pauseOnHover** Should autplay pause when the users mouse is over the carousel?

-}
type alias Settings =
    { amountOfSlides : Int
    , startingIndex : Int
    , slidesToShow : Int
    , slidesToScroll : Int
    , width : Float
    , height : Float
    , horizontal : Bool
    , infinite : Bool
    , transitionSpeedInMs : Int
    , easing : Float -> Float
    , friction : Float -> Float
    , threshold : Float
    , dragging : Bool
    , keyboard : Bool
    , scrollWheel : Bool
    , autoplay : Bool
    , autoplayIntervalInMs : Int
    , pauseOnHover : Bool
    }


{-| A record of default settings to start out with

    { amountOfSlides = 1
    , startingIndex = 0
    , slidesToShow = 1
    , slidesToScroll = 1
    , width = 500
    , height = 260
    , horizontal = True
    , infinite = True
    , transitionSpeedInMs = 500
    , threshold = 0.2
    , dragging = True
    , friction = identity
    , easing = identity
    , keyboard = True
    , scrollWheel = True
    , autoplay = False
    , autoplayIntervalInMs = 3000
    , pauseOnHover = True
    }

-}
default : Settings
default =
    { amountOfSlides = 1
    , startingIndex = 0
    , slidesToShow = 1
    , slidesToScroll = 1
    , width = 500
    , height = 260
    , horizontal = True
    , infinite = True
    , transitionSpeedInMs = 500
    , threshold = 0.2
    , friction = identity
    , easing = identity
    , dragging = True
    , keyboard = True
    , scrollWheel = True
    , autoplay = False
    , autoplayIntervalInMs = 3000
    , pauseOnHover = True
    }


{-| Helper to update the amount of slides on a Settings record
-}
updateAmountOfSlides : Int -> Settings -> Settings
updateAmountOfSlides amountOfSlides settings =
    { settings | amountOfSlides = amountOfSlides }


{-| Helper to update the width and heigh of your carousel on a Settings record
-}
updateWidthAndHeight : Float -> Float -> Settings -> Settings
updateWidthAndHeight width height settings =
    { settings | width = width, height = height }
