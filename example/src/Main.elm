module Main exposing (main)

import Browser exposing (element)
import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Ease
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Task
import Tricycle.Carousel as Carousel exposing (Carousel)
import Tricycle.Carousel.Settings as CarouselSettings


type alias Model =
    Maybe Carousel


type Msg
    = OnResize
    | OnReceiveElement (Result Dom.Error Dom.Element)
    | MsgForCarousel Carousel.Msg
    | Next
    | Previous


carouselSettings : CarouselSettings.Settings
carouselSettings =
    let
        default =
            CarouselSettings.default
    in
    { default
        | amountOfSlides = 10
        , easing = Ease.outCirc
        , friction = Ease.inCirc
        , threshold = 0.1
        , horizontal = True
    }


main : Program () Model Msg
main =
    element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing
    , Dom.getElement carouselWrapperId
        |> Task.attempt OnReceiveElement
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgForCarousel msgForCarousel ->
            Maybe.andThen (Just << Carousel.update msgForCarousel) model
                |> (\a -> ( a, Cmd.none ))

        Next ->
            Maybe.map Carousel.next model
                |> (\a -> ( a, Cmd.none ))

        Previous ->
            Maybe.map Carousel.previous model
                |> (\a -> ( a, Cmd.none ))

        OnReceiveElement (Ok { element }) ->
            let
                carousel =
                    case model of
                        Nothing ->
                            Carousel.init carouselSettings

                        Just c ->
                            c

                settings =
                    Carousel.toSettings carousel
                        |> (\s -> { s | height = 260, width = element.width })
                        |> (\s ->
                                if s.width < 400 then
                                    { s | slidesToShow = 1 }

                                else if s.width < 600 then
                                    { s | slidesToShow = 2 }

                                else
                                    { s | slidesToShow = 3 }
                           )
            in
            carousel
                |> Carousel.updateSettings settings
                |> (\a -> ( Just a, Cmd.none ))

        OnReceiveElement (Err err) ->
            ( model, Cmd.none )

        OnResize ->
            ( model, Task.attempt OnReceiveElement (Dom.getElement carouselWrapperId) )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Just carousel ->
            Sub.batch
                [ Carousel.subscriptions MsgForCarousel carousel
                , BrowserEvents.onResize (\_ _ -> OnResize)
                ]

        Nothing ->
            BrowserEvents.onResize (\_ _ -> OnResize)


view : Model -> Html Msg
view model =
    case model of
        Nothing ->
            Html.text "loading..."

        Just carousel ->
            viewCarousel carousel


viewCarousel : Carousel -> Html Msg
viewCarousel carousel =
    Html.div [ HtmlA.class "Carousel" ]
        [ Carousel.view MsgForCarousel (viewSlide (Carousel.toSettings carousel)) carousel
        , Html.div [ HtmlA.class "CarouselNav" ]
            [ Html.button [ HtmlE.onClick Previous ] [ Html.text "<" ]
            , Html.span [] []
            , Html.button [ HtmlE.onClick Next ] [ Html.text ">" ]
            ]
        ]


carouselWrapperId : String
carouselWrapperId =
    "CarouselWrapper"


viewSlide : CarouselSettings.Settings -> Carousel.SlideViewModel -> Html Msg
viewSlide { width, height } { index } =
    let
        sff =
            String.fromInt << round

        imageUrl =
            "https://picsum.photos/seed/" ++ ("aa" ++ String.fromInt index) ++ "/" ++ sff width ++ "/" ++ sff height
    in
    Html.div
        [ HtmlA.class "slide"
        , HtmlA.style "height" "100%"
        ]
        [ Html.img [ HtmlA.src imageUrl ] []
        ]
