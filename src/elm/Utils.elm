module Utils
    exposing
        ( (=>)
        , swap
        , indexedPair
        , indexedPair2
        , list
        )

{-| Additional basic functions.
# Tuples
@docs (=>), swap
-}


{-| A shorthand for writing 2-tuples. Very commonly used when expressing key/value pairs
in CSS or Json encoders.
-}
(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| Swaps the elements in a pair.
    swap ( 1, 2 ) == ( 2, 1 )
-}
swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


indexedPair : (a -> b) -> a -> ( b, a )
indexedPair f a =
    ( f a, a )


indexedPair2 : (a -> b) -> a -> ( a, b )
indexedPair2 f =
    swap << indexedPair f


list : a -> List a
list a =
    [ a ]
