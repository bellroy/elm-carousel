module Tricycle.Carousel.Internal.Indexes exposing
    ( Active
    , Index
    , Track
    , compare
    , fromActiveIndex
    , fromIndex
    , fromInt
    , map
    , toInt
    , toString
    , track
    )


type Index indexType
    = Index Int


type Active
    = Active


type Track
    = Track


fromInt : Int -> Index Track
fromInt =
    Index


track : Index a -> Index Track
track (Index int) =
    Index int


fromIndex : Index Track -> Index Active
fromIndex (Index int) =
    Index int


toInt : Index a -> Int
toInt (Index int) =
    int


toString : Index a -> String
toString =
    toInt >> String.fromInt


fromActiveIndex : Index Active -> Index Track
fromActiveIndex (Index int) =
    Index int


compare : Index a -> Int -> Basics.Order
compare (Index a) b =
    Basics.compare a b


map : (Int -> Int) -> Index a -> Index a
map f (Index a) =
    Index (f a)
