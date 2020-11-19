module Tricycle.Carousel.Internal.View exposing (view)

import Html exposing (Attribute, Html)
import Html.Attributes as HtmlA
import Html.Keyed as HtmlK
import Html.Lazy as HtmlL
import Json.Decode as Decode exposing (Decoder)
import Tricycle.Carousel.Internal.Calculations as Calc
import Tricycle.Carousel.Internal.Distance as Distance
import Tricycle.Carousel.Internal.Indexes as Indexes exposing (Active, Index)
import Tricycle.Carousel.Internal.Movement exposing (Movement(..))
import Tricycle.Carousel.Internal.Msg exposing (Msg(..))
import Tricycle.Carousel.Internal.Position as Position exposing (Position)
import Tricycle.Carousel.Internal.State as State exposing (State)
import Tricycle.Carousel.Settings exposing (Settings)
import VirtualDom as VDom


type alias SlideViewModel =
    { index : Int
    , isActive : Bool
    , isInView : Bool
    }


view : (Msg -> msg) -> (SlideViewModel -> Html msg) -> Settings -> State -> Html msg
view wrapMsg renderSlide settings state =
    viewSlides renderSlide settings state
        |> viewTrack wrapMsg settings state
        |> viewInner settings
        |> viewWrapper wrapMsg settings


viewSlides : (SlideViewModel -> Html msg) -> Settings -> State -> List ( String, Html msg )
viewSlides renderSlide settings state =
    (if settings.infinite then
        List.range
            (negate <| Calc.clonesAmountPerSide settings)
            (settings.amountOfSlides + Calc.clonesAmountPerSide settings - 1)

     else
        List.range 0 (settings.amountOfSlides - 1)
    )
        |> List.map
            (\i ->
                ( String.fromInt i
                , HtmlL.lazy
                    (\_ -> viewSlide renderSlide settings state i)
                    state.activeIndex
                )
            )


viewSlide : (SlideViewModel -> Html msg) -> Settings -> State -> Int -> Html msg
viewSlide renderSlide settings state trackIndex =
    let
        index =
            modBy settings.amountOfSlides (trackIndex + settings.amountOfSlides)

        isActive =
            indexIsActive trackIndex

        indexIsActive i =
            case Indexes.compare state.activeIndex i of
                EQ ->
                    True

                _ ->
                    False

        isInView =
            isActive
                || ((trackIndex - Indexes.toInt state.activeIndex)
                        >= 0
                        && (trackIndex - Indexes.toInt state.activeIndex)
                        < settings.slidesToShow
                   )

        viewModel =
            { index = index
            , isActive = isActive
            , isInView = isInView
            }
    in
    Html.div
        ([ [ HtmlA.attribute "data-track-index" (String.fromInt trackIndex)
           , HtmlA.attribute "data-index" (String.fromInt index)
           ]
         , [ HtmlA.style "box-sizing" "border-box"
           , HtmlA.style "flex" "1 0 auto"
           ]
         , if settings.horizontal then
            [ HtmlA.style "height" (px <| settings.height)
            , HtmlA.style "width" (px <| Calc.slideLength settings)
            ]

           else
            [ HtmlA.style "height" (px <| Calc.slideLength settings)
            , HtmlA.style "width" (px <| settings.width)
            ]
         ]
            |> List.concat
        )
        [ renderSlide viewModel
        ]


viewTrack : (Msg -> msg) -> Settings -> State -> List ( String, Html msg ) -> Html msg
viewTrack wrapMsg settings state =
    let
        transform =
            let
                makeTranslate p =
                    let
                        pf =
                            Position.toFloat p
                    in
                    if settings.horizontal then
                        "translate3d("
                            ++ px pf
                            ++ ", 0, 0)"

                    else
                        "translate3d(0,"
                            ++ px pf
                            ++ ", 0)"
            in
            State.currentPositionIncludingDrag settings state.movement state.currentPosition
                |> makeTranslate
    in
    HtmlK.node "div"
        (List.concat
            [ [ HtmlA.style "position" "relative"
              , HtmlA.style "top" "0"
              , HtmlA.style "left" "0"
              , HtmlA.style "display" "flex"
              , if settings.horizontal then
                    HtmlA.style "flex-direction" "row"

                else
                    HtmlA.style "flex-direction" "column"
              , HtmlA.style "willChange" "transform"
              ]
            , if settings.horizontal then
                [ HtmlA.style "width" (px <| Calc.trackLength settings)
                , HtmlA.style "height" (px <| settings.height)
                ]

              else
                [ HtmlA.style "width" (px <| settings.width)
                , HtmlA.style "height" (px <| Calc.trackLength settings)
                ]
            , [ HtmlA.style "transform" transform
              ]
            , (case state.movement of
                None ->
                    [ dragStartEvents wrapMsg state settings
                    , scrollWheelEvents wrapMsg settings
                    ]

                ByAnimation _ _ ->
                    [ dragStartEvents wrapMsg state settings
                    , scrollWheelEvents wrapMsg settings
                    ]

                ByDragging startIndex startPosition _ ->
                    [ whileDraggingEvents wrapMsg settings startIndex startPosition
                    , scrollWheelEvents wrapMsg settings
                    ]
              )
                |> List.concat
            ]
        )


viewInner : Settings -> Html msg -> Html msg
viewInner settings =
    List.singleton
        >> Html.div
            [ HtmlA.style "display" "block"
            , HtmlA.style "position" "relative"
            , HtmlA.style "overflow" "hidden"
            , HtmlA.style "width" (px <| settings.width)
            , HtmlA.style "height" (px <| settings.height)
            , HtmlA.style "margin" "0"
            , HtmlA.style "padding" "0"
            ]


viewWrapper : (Msg -> msg) -> Settings -> Html msg -> Html msg
viewWrapper wrapMsg settings =
    List.singleton
        >> Html.div
            [ HtmlA.style "width" (px <| settings.width)
            , HtmlA.style "height" (px <| settings.height)
            , on "mouseenter" (Decode.succeed <| wrapMsg <| OnMouseEnter) False
            , on "mouseleave" (Decode.succeed <| wrapMsg <| OnMouseLeave) False
            ]



---


px : Float -> String
px =
    String.fromFloat >> (\a -> a ++ "px")



--


dragStartEvents : (Msg -> msg) -> State -> Settings -> List (Attribute msg)
dragStartEvents wrapMsg state settings =
    if settings.dragging then
        let
            decodeDragStart =
                Decode.map
                    (wrapMsg << DragStart state.activeIndex)
                    (Position.decodePagePosition settings)
        in
        [ on "mousedown" decodeDragStart False
        , on "touchstart" decodeDragStart False
        , on "dragstart" decodeDragStart False
        ]

    else
        []


whileDraggingEvents : (Msg -> msg) -> Settings -> Index Active -> Position -> List (Attribute msg)
whileDraggingEvents wrapMsg settings startIndex startPosition =
    let
        decodeDragging =
            Decode.map (DragMove startIndex startPosition >> wrapMsg)
                (Position.decodePagePosition settings)

        decodeDragEnd =
            Decode.map
                (Distance.fromPositions startPosition >> DragEnd startIndex >> wrapMsg)
                (Position.decodePagePosition settings)
    in
    [ on "mousemove" decodeDragging True
    , on "touchmove" decodeDragging True

    --
    , on "mouseup" decodeDragEnd False
    , on "mouseleave" decodeDragEnd False
    , on "touchend" decodeDragEnd False

    --
    , on "touchcancel" decodeDragEnd False
    , on "touch" decodeDragEnd False
    ]


scrollWheelEvents : (Msg -> msg) -> Settings -> List (Attribute msg)
scrollWheelEvents wrapMsg settings =
    let
        decodeDeltaPositionForField fieldName =
            Decode.field (fieldName ++ "Y") <| Decode.float

        decodeDelta =
            Decode.oneOf
                [ decodeDeltaPositionForField "delta"
                , decodeDeltaPositionForField "wheelDelta"
                ]
                |> Decode.map
                    (Position.fromFloat
                        >> Distance.fromPositions (Position.fromFloat 0)
                        >> OnScrollWheel
                        >> wrapMsg
                    )
    in
    if settings.scrollWheel then
        [ on "wheel" decodeDelta True ]

    else
        []


on : String -> Decoder msg -> Bool -> Attribute msg
on eventName decoder preventDefault =
    let
        handler =
            if preventDefault then
                Decode.map2 Tuple.pair
                    decoder
                    (Decode.maybe (Decode.field "cancelable" Decode.bool)
                        |> Decode.andThen (Decode.succeed << Maybe.withDefault True)
                    )
                    |> VDom.MayPreventDefault

            else
                VDom.Normal decoder
    in
    VDom.on eventName handler
