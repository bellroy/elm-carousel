module Tricycle.Carousel exposing
    ( Carousel, Msg, SlideViewModel, init, subscriptions, update, view
    , getCurrentIndex, next, previous, goTo, moveTo
    , toSettings, updateSettings
    )

{-|

@docs Carousel, Msg, SlideViewModel, init, subscriptions, update, view


## Navigate

@docs getCurrentIndex, next, previous, goTo, moveTo


## Settings

@docs toSettings, updateSettings

-}

import Html exposing (Html)
import Tricycle.Carousel.Internal.Indexes as Indexes
import Tricycle.Carousel.Internal.Msg as Msg
import Tricycle.Carousel.Internal.State as State exposing (State)
import Tricycle.Carousel.Internal.Subscriptions as Subscriptions
import Tricycle.Carousel.Internal.View as View
import Tricycle.Carousel.Settings exposing (Settings)


{-| -}
type Carousel
    = Carousel Settings State


{-| -}
type alias SlideViewModel =
    { index : Int
    , isActive : Bool
    , isInView : Bool
    }


{-| -}
type alias Msg =
    Msg.Msg


{-| Initialize a new Carousel with the given Settings
-}
init : Settings -> Carousel
init settings =
    State.init settings
        |> Carousel settings


{-| Update your Carousel

    import Tricycle.Carousel as Carousel

    type Msg
        = MsgForCarousel Carousel.Msg
        | Foo

    type alias Model = {
        carousel: Carousel.Carousel,
        foo: String
    }

    update: Msg -> Model -> (Model, Cmd msg)
    update msg model =
        case msg of
            MsgForCarousel carouselMsg ->
                ({ model | carousel = Carousel.update carouselMsg model.carousel }, Cmd.none)

            Foo ->
                { Model | foo = "bar" }

-}
update : Msg -> Carousel -> Carousel
update msg (Carousel settings state) =
    Carousel settings (State.update settings msg state)


{-| It's important to subscribe to the Carousel's subscriptions in your app.

    import Tricycle.Carousel as Carousel

    type Msg
        = MsgForCarousel Carousel.Msg
        | Foo

    type alias Model =
        { carousel : Carousel.Carousel
        , foo : String
        }

    subscriptions : Model -> Sub Msg
    subscriptions model =
        Carousel.subscriptions wrapMsg model.carousel

-}
subscriptions : (Msg -> msg) -> Carousel -> Sub msg
subscriptions wrapMsg (Carousel settings state) =
    Subscriptions.subscriptions settings state
        |> Sub.map wrapMsg


{-| Render your Carousel

  - Supply a function to wrap the Carousel's internal Msgs with
  - Supply a function that takes a SlideViewModel and return the Html for the requested Slide

-}
view : (Msg -> msg) -> (SlideViewModel -> Html msg) -> Carousel -> Html msg
view wrapMsg renderSlide (Carousel settings state) =
    View.view wrapMsg renderSlide settings state


{-| Update the settings of the Carousel

This includes things like the width and height of your Carousel and the amount of slides.

-}
updateSettings : Settings -> Carousel -> Carousel
updateSettings settings (Carousel _ state) =
    Carousel settings state
        |> update Msg.Reset


{-| Returns the settings currently being used by the Carousel
-}
toSettings : Carousel -> Settings
toSettings (Carousel settings _) =
    settings


{-| Get the current active index
-}
getCurrentIndex : Carousel -> Int
getCurrentIndex (Carousel settings state) =
    if settings.infinite then
        modBy settings.amountOfSlides (Indexes.toInt state.activeIndex + settings.amountOfSlides)

    else
        Indexes.toInt state.activeIndex


{-| Go to the next slide
-}
next : Carousel -> Carousel
next =
    update Msg.Next


{-| Go to the previous slide
-}
previous : Carousel -> Carousel
previous =
    update Msg.Previous


{-| Go to slide index x

Skips transition

-}
goTo : Int -> Carousel -> Carousel
goTo index =
    Indexes.fromInt index
        |> Msg.WarpToIndex
        |> update


{-| Move to slide index x
-}
moveTo : Int -> Carousel -> Carousel
moveTo index =
    Indexes.fromInt index
        |> Msg.AnimateToIndex
        |> update
