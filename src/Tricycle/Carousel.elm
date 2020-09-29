module Tricycle.Carousel exposing (..)

import Animation exposing (Animation)
import Browser.Events as BrowserEvents
import Ease
import Html exposing (Attribute, Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Decode exposing (Decoder)


type Carousel
    = Carousel CarouselRecord


type alias CarouselRecord =
    { amountOfSlides : Int
    , slidesToShow : Int
    , slidesToScroll : Int
    , width : Float
    , height : Float
    , horizontal : Bool
    , infinite : Bool

    --
    , transitionSpeedInMs : Int
    , easing : Float -> Float

    -- dragging
    , friction : Float -> Float
    , threshold : Float

    -- , initialIndex : Int
    , activeIndex : Int

    --
    , clock : Float
    , currentPosition : Position
    , desiredPosition : Maybe Position
    , animation :
        { x : Maybe Animation
        , y : Maybe Animation
        }
    , drag :
        Maybe
            { original : Position
            , start : Position
            , current : Position
            }
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Timeline =
    List Position


type Msg
    = OnAnimationFrameDelta Float
    | Reset
    | MoveToIndex Int
      --
    | DragStart Position
    | DragMove Position Position
    | DragEnd Position


init : Carousel
init =
    let
        carouselRecord =
            { amountOfSlides = 7
            , slidesToShow = 3
            , slidesToScroll = 1
            , width = 500
            , height = 260
            , horizontal = True
            , infinite = True
            , transitionSpeedInMs = 1000
            , threshold = 0.1
            , friction = Ease.inQuad
            , easing = Ease.outQuart
            , activeIndex = 0
            , clock = 0
            , currentPosition = { x = 0, y = 0 }
            , desiredPosition = Nothing
            , animation = { x = Nothing, y = Nothing }
            , drag = Nothing
            }
    in
    Carousel
        { carouselRecord
            | currentPosition =
                calcPositionForIndex carouselRecord carouselRecord.activeIndex
        }


update : Msg -> Carousel -> Carousel
update msg ((Carousel r) as carousel) =
    case msg of
        OnAnimationFrameDelta delta ->
            let
                updatedClock =
                    r.clock + delta

                keepAnimation =
                    Maybe.andThen
                        (\a_ ->
                            if Animation.isDone updatedClock a_ then
                                Nothing

                            else
                                Just a_
                        )

                fromAnimation =
                    Maybe.map (Animation.animate updatedClock)
            in
            { r
                | clock = updatedClock
                , animation =
                    { x = keepAnimation r.animation.x
                    , y = keepAnimation r.animation.y
                    }
                , currentPosition =
                    { x = fromAnimation r.animation.x |> Maybe.withDefault r.currentPosition.x
                    , y = fromAnimation r.animation.y |> Maybe.withDefault r.currentPosition.y
                    }
            }
                |> Carousel

        Reset ->
            update (MoveToIndex r.activeIndex) carousel

        MoveToIndex index ->
            let
                desiredPosition =
                    calcPositionForIndex r index
            in
            if desiredPosition == r.currentPosition then
                carousel

            else
                let
                    correctedDurationHorizontal =
                        (toFloat r.transitionSpeedInMs / r.width)
                            * (desiredPosition.x - r.currentPosition.x)
                            |> abs

                    correctedDurationVertical =
                        (toFloat r.transitionSpeedInMs / r.height)
                            * (desiredPosition.y - r.currentPosition.y)
                            |> abs
                in
                Carousel
                    { r
                        | desiredPosition = Just desiredPosition
                        , activeIndex = index
                        , animation =
                            { x =
                                Animation.animation r.clock
                                    |> Animation.from r.currentPosition.x
                                    |> Animation.to desiredPosition.x
                                    |> Animation.ease r.easing
                                    |> Animation.duration correctedDurationHorizontal
                                    |> Just
                            , y =
                                Animation.animation r.clock
                                    |> Animation.from r.currentPosition.y
                                    |> Animation.to desiredPosition.y
                                    |> Animation.ease r.easing
                                    |> Animation.duration correctedDurationVertical
                                    |> Just
                            }
                    }

        DragStart start ->
            Carousel
                { r
                    | drag =
                        Just
                            { original = r.currentPosition
                            , start = start
                            , current = start
                            }
                    , animation =
                        { x = Nothing
                        , y = Nothing
                        }
                }

        DragMove start current ->
            case r.drag of
                Nothing ->
                    update (DragStart start) carousel

                Just drag ->
                    let
                        distance =
                            getPos r start - getPos r current

                        updatedPositionZ =
                            getPos r drag.original
                                - calcDistanceWithFrictionInLength
                                    r.friction
                                    (getLength r)
                                    distance
                    in
                    if
                        not r.infinite
                            && ((r.activeIndex == 0 && distance <= 0)
                                    || (r.activeIndex == (r.amountOfSlides - 1) && distance >= 0)
                               )
                    then
                        carousel

                    else
                        let
                            updatedPosition =
                                if r.horizontal then
                                    { x = updatedPositionZ
                                    , y = r.currentPosition.y
                                    }

                                else
                                    { x = r.currentPosition.x
                                    , y = updatedPositionZ
                                    }
                        in
                        Carousel
                            { r
                                | drag = Just { drag | start = start, current = current }
                                , animation = { x = Nothing, y = Nothing }
                                , currentPosition = updatedPosition
                            }

        DragEnd _ ->
            case r.drag of
                Nothing ->
                    carousel

                Just { original } ->
                    let
                        ratioTraveled =
                            if r.horizontal then
                                (1 / r.width) |> (*) (original.x - r.currentPosition.x)

                            else
                                (1 / r.height) |> (*) (original.y - r.currentPosition.y)
                    in
                    if abs ratioTraveled > r.threshold then
                        let
                            newIndex =
                                if ratioTraveled < 0 then
                                    r.activeIndex - 1

                                else
                                    r.activeIndex + 1

                            desiredPosition =
                                calcPositionForIndex r newIndex

                            updatedCarousel =
                                Carousel
                                    { r
                                        | drag = Nothing
                                        , desiredPosition = Just desiredPosition
                                        , activeIndex = newIndex
                                    }
                        in
                        update (MoveToIndex newIndex) updatedCarousel

                    else
                        Carousel { r | drag = Nothing }
                            |> update Reset


subscriptions : Carousel -> Sub Msg
subscriptions _ =
    BrowserEvents.onAnimationFrameDelta OnAnimationFrameDelta


px : Float -> String
px =
    String.fromFloat >> (\a -> a ++ "px")


view : Carousel -> Html Msg
view (Carousel r) =
    let
        wrapper el =
            Html.div
                [ HtmlA.style "margin" "50px auto"
                , HtmlA.style "width" (px <| r.width)
                , HtmlA.style "height" (px <| r.height)
                , HtmlA.style "background-image" <|
                    "linear-gradient("
                        ++ (if r.horizontal then
                                "90deg"

                            else
                                "0deg"
                           )
                        ++ ", blue 0%, blue "
                        ++ (String.fromFloat <| r.threshold * 100)
                        ++ "%, white "
                        ++ (String.fromFloat <| r.threshold * 100)
                        ++ "%, white "
                        ++ (String.fromFloat <| 100 - r.threshold * 100)
                        ++ "%, blue "
                        ++ (String.fromFloat <| 100 - r.threshold * 100)
                        ++ "%, blue 100%)"
                ]
                [ el ]

        inner el =
            Html.div
                [ HtmlA.style "display" "block"
                , HtmlA.style "position" "relative"

                -- , HtmlA.style "overflow" "hidden"
                , HtmlA.style "width" (px <| r.width)
                , HtmlA.style "height" (px <| r.height)

                -- , HtmlA.style "border" "10px solid purple"
                , HtmlA.style "margin" "0"
                , HtmlA.style "padding" "0"
                ]
                [ el ]

        track =
            Html.div
                (List.concat
                    [ [ HtmlA.style "position" "relative"
                      , HtmlA.style "top" "0"
                      , HtmlA.style "left" "0"
                      , HtmlA.style "display" "flex"
                      , if r.horizontal then
                            HtmlA.style "flex-direction" "row"

                        else
                            HtmlA.style "flex-direction" "column"
                      , HtmlA.style "willChange" "transform"
                      ]
                    , if r.horizontal then
                        [ HtmlA.style "width" (px <| calcTrackLength r)
                        , HtmlA.style "height" (px <| r.height)
                        ]

                      else
                        [ HtmlA.style "width" (px <| r.width)
                        , HtmlA.style "height" (px <| calcTrackLength r)
                        ]
                    , [ HtmlA.style "transform" <|
                            "translate3d("
                                ++ px r.currentPosition.x
                                ++ ","
                                ++ px r.currentPosition.y
                                ++ ", 0)"
                      ]
                    , dragEvents r.drag
                    ]
                )

        ( rangeFrom, rangeTo ) =
            if r.infinite then
                ( negate <| calcClonesAmountPerSide r
                , r.amountOfSlides + calcClonesAmountPerSide r - 1
                )

            else
                ( 0, r.amountOfSlides - 1 )

        slides =
            List.range rangeFrom rangeTo
                |> List.map
                    (\trackIndex ->
                        let
                            index =
                                modBy r.amountOfSlides (trackIndex + r.amountOfSlides)

                            isActive =
                                trackIndex == r.activeIndex

                            -- isInViewport =
                            --     isActive
                            --         || ((activeIndex - trackIndex |> negate) > slidesInView)
                        in
                        Html.div
                            ([ [ -- , HtmlA.style "flex" "1 0 auto"
                                 if isActive then
                                    HtmlA.style "border" "10px solid green"

                                 else
                                    HtmlA.style "border" "2px solid black"
                               , HtmlA.style "box-sizing" "border-box"
                               , HtmlA.style "flex" "1 0 auto"
                               ]
                             , if r.horizontal then
                                [ HtmlA.style "height" (px <| r.height)
                                , HtmlA.style "width" (px <| calcSlideLength r)
                                ]

                               else
                                [ HtmlA.style "height" (px <| calcSlideLength r)
                                , HtmlA.style "width" (px <| r.width)
                                ]
                             ]
                                |> List.concat
                            )
                            [ String.fromInt index |> Html.text ]
                    )
    in
    slides
        |> track
        |> inner
        |> wrapper


dragEvents : Maybe { original : Position, start : Position, current : Position } -> List (Attribute Msg)
dragEvents maybeDrag =
    let
        decodePageXY =
            let
                decodePageXY_ =
                    Decode.map2 Position
                        (Decode.field "pageX" Decode.float)
                        (Decode.field "pageY" Decode.float)
            in
            Decode.oneOf
                [ decodePageXY_
                , Decode.at [ "touches", "0" ] decodePageXY_
                ]

        decodeDragStart =
            Decode.map DragStart decodePageXY

        decodeDragging =
            Decode.map
                (\current ->
                    case maybeDrag of
                        Just { start } ->
                            DragMove start current

                        Nothing ->
                            DragMove current current
                )
                decodePageXY

        decodeDragEnd =
            Decode.map
                (\current ->
                    case maybeDrag of
                        Just { start } ->
                            DragEnd
                                { x = start.x - current.x
                                , y = start.y - current.y
                                }

                        Nothing ->
                            DragEnd current
                )
                decodePageXY

        decodeReset =
            Decode.succeed Reset
    in
    case maybeDrag of
        Nothing ->
            [ on "mousedown" decodeDragStart False
            , on "touchstart" decodeDragStart False
            ]

        _ ->
            [ on "mousemove" decodeDragging True
            , on "touchmove" decodeDragging True

            --
            , on "mouseup" decodeDragEnd False
            , on "mouseleave" decodeDragEnd False
            , on "dragleave" decodeDragEnd False
            , on "dragend" decodeDragEnd False
            , on "touchend" decodeDragEnd False

            --
            , on "touchcancel" decodeReset False
            , on "click" decodeReset False
            ]


on : String -> Decoder Msg -> Bool -> Attribute Msg
on eventName decoder preventDefault =
    if preventDefault then
        decoder
            |> Decode.map (\a -> ( a, True ))
            |> HtmlE.preventDefaultOn eventName

    else
        HtmlE.on eventName decoder



--


getPos : { a | horizontal : Bool } -> (Position -> Float)
getPos { horizontal } =
    if horizontal then
        .x

    else
        .y


getLength : CarouselRecord -> Float
getLength { horizontal, width, height } =
    if horizontal then
        width

    else
        height


calcPositionForIndex : CarouselRecord -> Int -> Position
calcPositionForIndex ({ width, height, horizontal, slidesToShow } as args) index =
    let
        forLength length =
            calcClonesAmountPerSide args
                |> (+) index
                |> (-) 0
                |> toFloat
                |> (*) length
                |> (\a -> a / (toFloat <| slidesToShow))

        x =
            if horizontal then
                forLength width

            else
                0

        y =
            if horizontal then
                0

            else
                forLength height
    in
    { x = x, y = y }


calcClonesAmountPerSide : CarouselRecord -> Int
calcClonesAmountPerSide { slidesToShow, slidesToScroll, infinite } =
    if infinite then
        slidesToShow + slidesToScroll

    else
        0


calcSlideLength : CarouselRecord -> Float
calcSlideLength ({ slidesToShow } as carouselRecord) =
    slidesToShow
        |> toFloat
        |> (/) (getLength carouselRecord)


calcTrackLength : CarouselRecord -> Float
calcTrackLength ({ amountOfSlides } as carouselRecord) =
    toFloat (amountOfSlides + calcClonesAmountPerSide carouselRecord)
        * calcSlideLength carouselRecord


calcDistanceWithFrictionInLength : (Float -> Float) -> Float -> Float -> Float
calcDistanceWithFrictionInLength friction length distance =
    (1 / length)
        |> (*) distance
        |> abs
        |> friction
        |> (*) distance
